%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.bat.m.
% Main author: schwering.
%
% Fuzzy layer ontop of the RSTC.
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.bat.

:- interface.

%-----------------------------------------------------------------------------%

:- func follow(agent, agent) `with_type` rstc.proc(N) <= arithmetic(N).
:- func tailgate(agent, agent) `with_type` rstc.proc(N) <= arithmetic(N).
:- func overtake(agent, agent) `with_type` rstc.proc(N) <= arithmetic(N).

%-----------------------------------------------------------------------------%

:- instance bat(prim(N)) <= arithmetic(N).
:- instance obs_bat(prim(N), obs) <= arithmetic(N).
:- instance pr_bat(prim(N), obs, env) <= arithmetic(N).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module domain.car.rstc.fuzzy.
:- import_module prgolog.fluent.
:- import_module assoc_list.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- func accelf(agent, func(rstc.sit(N)) = N) `with_type` primf(rstc.prim(N))
    <= arithmetic(N).
:- mode accelf(in, func(in) = out is det, in) = out is det.
:- mode accelf(in, func(in) = out is semidet, in) = out is det.

accelf(B, AccelF, S) = ( if Q = AccelF(S) then accel(B, Q) else abort ).


:- func lcf(agent) `with_type` primf(rstc.prim(N)) <= arithmetic(N).

lcf(B, S) = ( if lane(B, S) = left then lc(B, right) else lc(B, left) ).


:- func ntg_after(agent, agent, rstc.sit(N), s(N)) = ntg(N) <= arithmetic(N).
:- mode ntg_after(in, in, in, in) = out is semidet.

ntg_after(B, C, S, T) = ntg(B, C, prgolog.do(wait(T), S)).


:- func ttc_after(agent, agent, rstc.sit(N), s(N)) = ttc(N) <= arithmetic(N).
:- mode ttc_after(in, in, in, in) = out is semidet.

ttc_after(B, C, S, T) = ttc(B, C, prgolog.do(wait(T), S)).


:- func max_search_time = N <= arithmetic(N).

max_search_time = from_int(60).


:- func wait_until(func(rstc.sit(N)) = s(N), s(N))
    `with_type` primf(rstc.prim(N)) <= arithmetic(N).
:- mode wait_until(in(func(in) = out is semidet), in, in) = out is det.
:- mode wait_until(in(func(in) = out is det), in, in) = out is det.

wait_until(F, Goal, S) = A :-
    if   bin_search(func(T) = F(prgolog.do(wait(T), S)) is semidet,
                    zero, max_search_time, Goal, V1)
    then A = wait(V1)
    else A = abort.

%-----------------------------------------------------------------------------%

follow(B, Victim) = P :-
    P = t(r((pred(S::in) is semidet :-
            ntg(B, Victim, S) `in` close_behind))
        ) `;`
        b(accelf(B, rel_v(Victim, B))).


tailgate(B, Victim) = P :-
    P = t(r((pred(S::in) is semidet :-
            ntg(B, Victim, S) `in_any` [very_close_behind, very_close_behind]))
        ) `;`
        b(accelf(B, rel_v(Victim, B))).


overtake(B, Victim) = P :-
    P = t(r((pred(S::in) is semidet :-
            ntg(B, Victim, S) `in` close_behind))
        ) `;`
        b(accelf(B, rel_v(Victim, B))).

%-----------------------------------------------------------------------------%

:- pred poss(prim(N), rstc.sit(N)) <= arithmetic(N).
:- mode poss(in, in) is semidet.
:- mode poss(in(wait), in) is semidet.
:- mode poss(in(accel), in) is semidet.
:- mode poss(in(lc), in) is semidet.
:- mode poss(in(senseD), in) is det.
:- mode poss(in(senseL), in) is det.
:- mode poss(in(init_env), in) is det.
:- mode poss(in(seed), in) is det.
:- mode poss(in(abort), in) is failure.

poss(wait(T), _) :- T >= zero.
poss(accel(_, Q), _) :- Q >= zero.
poss(lc(B, L), S) :- lane(B, S) = left <=> L = right.
%poss(lc(B, L), lc(B, L), S) :- abs(lane(B, S) - L) = 1.
poss(senseD(_, _, _, _), _).
poss(senseL(_, _), _).
poss(init_env(_), _).
poss(match(obs(_, D)), S) :- match_obs(D, S).
poss(seed(_), _).
poss(abort, _) :- fail.

%-----------------------------------------------------------------------------%

:- func lookahead(rstc.sit(N)) = lookahead is det.

lookahead(_S) = 3.

%-----------------------------------------------------------------------------%

:- func reward(rstc.sit(N)) = reward.

reward(s0) = 0.0.
reward(do(A, S)) = ( if A = match(_) then 1.0 else 0.0 ) + reward(S).


:- func reward(rstc.prog(N), rstc.sit(N)) = reward.

reward(_, S) = reward(S).

%-----------------------------------------------------------------------------%

:- pred match_obs(assoc_list(agent, info)::in, rstc.sit(N)::in) is semidet
    <= arithmetic(N).

match_obs(L, S) :-
    all [B, C] (
        if      member(PB @ (B - _), L),
                member(PC @ (C - _), L)
        then    match_info(PB, PC, S)
        else    true
    ),
    all [D] (
        if      member(P @ (D - _), L)
        then    match_y(P, S)
        else    true
    ).


:- pred match_info(pair(agent, info)::in, pair(agent, info)::in,
                   rstc.sit(N)::in) is semidet <= arithmetic(N).

match_info((B - info(VB, _, p(XB, _))), (C - info(VC, _, p(XC, _))), S) :-
    NTG2 = from_float((XC - XB) / VB) `with_type` N,
    TTC2 = from_float((XC - XB) / (VB - VC)) `with_type` N,
    (   if      NTG1 = ntg(B, C, S)
        then    some [Cat] (
                    ntg_cat(Cat),
                    NTG1 `in` Cat,
                    NTG2 `in` Cat
                )
        else    true
    ),
    (   if      TTC1 = ttc(B, C, S)
        then    some [Cat] (
                    ttc_cat(Cat),
                    TTC1 `in` Cat,
                    TTC2 `in` Cat
                )
        else    true
    ).


:- pred match_y(pair(agent, info)::in, rstc.sit(N)::in) is semidet
    <= arithmetic(N).

match_y((B - info(_, _, p(_, Y))), S) :-
    Lane = lane(B, S),
    (   Lane = right, Y `float.'<'` 0.0
    ;   Lane = left,  Y `float.'>'` 0.0
    ).

%-----------------------------------------------------------------------------%

:- pred is_obs_prog(pseudo_atom(prim(N))::in) is semidet <= arithmetic(N).

is_obs_prog(complex(seq(pseudo_atom(atom(primf(_))),
                        pseudo_atom(atom(prim(match(_))))))).

%-----------------------------------------------------------------------------%

:- func obs_to_action(obs) = pseudo_atom(prim(N)) <= arithmetic(N).

obs_to_action(Obs @ obs(T, _)) = complex(seq(pseudo_atom(atom(Wait)),
                                             pseudo_atom(atom(Match)))) :-
    T1 = from_float(T),
    Wait = primf(func(S) = wait(T1 - start(S))),
    Match = prim(match(Obs)).

%-----------------------------------------------------------------------------%

:- instance bat(prim(N)) <= arithmetic(N) where [
    pred(poss/2) is bat.poss,
    func(reward/2) is bat.reward,
    func(lookahead/1) is bat.lookahead
].

:- instance obs_bat(prim(N), obs) <= arithmetic(N) where [
    (is_obs_action(match(_))),
    (covered_by_obs(do(A, S)) :-
        (   A \= wait(_), covered_by_obs(S)
        ;   A = match(_) )
    ),
    pred(is_obs_prog/1) is bat.is_obs_prog,
    func(obs_to_action/1) is bat.obs_to_action
].

:- instance pr_bat(prim(N), obs, env) <= arithmetic(N) where [
    seed_init_sit(I) = do(seed(I), s0),
    init_env_sit(env(T, Map), S) = do(init_env(env(T, Map)), S)
].

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.bat.
%-----------------------------------------------------------------------------%

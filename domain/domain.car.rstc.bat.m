%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012-2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
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

:- func rel_v(agent, agent, rstc.sit(N)) = scale(N) <= arithmetic.arithmetic(N).
:- mode rel_v(in, in, in) = out is semidet.

:- pred opposite_direction(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic.arithmetic(N).

:- pred same_direction(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic.arithmetic(N).

:- pred slower(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic.arithmetic(N).

:- pred faster(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic.arithmetic(N).

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%

:- func follow(agent, agent) `with_type` rstc.proc(N)
    <= arithmetic.arithmetic(N).

:- func tailgate(agent, agent) `with_type` rstc.proc(N)
    <= arithmetic.arithmetic(N).

:- func overtake(agent, agent) `with_type` rstc.proc(N)
    <= arithmetic.arithmetic(N).

:- func imitate(agent, agent) `with_type` rstc.proc(N)
    <= arithmetic.arithmetic(N).

%-----------------------------------------------------------------------------%

:- instance bat(prim(N)) <= arithmetic.arithmetic(N).
:- instance obs_bat(prim(N), obs) <= arithmetic.arithmetic(N).
:- instance pr_bat(prim(N), obs, env) <= arithmetic.arithmetic(N).

%-----------------------------------------------------------------------------%

:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module domain.car.rstc.fuzzy.
:- import_module prgolog.fluent.
:- import_module assoc_list.
:- import_module list.
:- import_module pair.
:- import_module util.simulated_annealing.

%-----------------------------------------------------------------------------%

:- pragma memo(reward/1, [allow_reset, statistics, fast_loose]). 

%-----------------------------------------------------------------------------%

rel_v(C, B, S) = one - ntg(B, C, S) / ttc(B, C, S).

opposite_direction(B, C, S) :- rel_v(B, C, S) > zero.

same_direction(B, C, S) :- rel_v(B, C, S) < zero.

slower(B, C, S) :- rel_v(B, C, S) > -one, rel_v(B, C, S) < one.

faster(B, C, S) :- not slower(B, C, S).

%-----------------------------------------------------------------------------%

:- pred actions_since(pred(A)::in(pred(in) is semidet),
                      prgolog.sit(A)::in, A::out) is nondet.

actions_since(P, do(A, S), A1) :-
    not P(A),
    (   actions_since(P, S, A1)
    ;   A = A1
    ).


:- func actions_since(pred(A)::in(pred(in) is semidet),
                      prgolog.sit(A)::in) = (list(A)::out) is det.

actions_since(P, S1) =
    ( if S1 = do(A, S), not P(A) then actions_since(P, S) ++ [A] else [] ).


:- pred to_be_copied(agent::in, agent::in, rstc.sit(N)::in,
                     rstc.prim(N)::out) is nondet.

to_be_copied(Copycat, Actor, S, A) :-
    BStart = (pred(start(B, _)::in) is semidet :- B = Copycat),
    actions_since(BStart, S, A),
    ( A = accel(Actor, _) ; A = lc(Actor, _) ).


:- func to_be_copied(agent, agent, rstc.sit(N)) = list(rstc.prim(N)).

to_be_copied(Copycat, Actor, S) = filter(P, actions_since(Q, S)) :-
    Q = (pred(start(B, _)::in) is semidet :- B = Copycat),
    P = (pred(A::in) is semidet :- ( A = accel(Actor, _) ; A = lc(Actor, _) )).


:- pred next_to_be_copied(agent::in, agent::in, rstc.sit(N)::in,
                          rstc.prim(N)::out) is semidet.

next_to_be_copied(Copycat, Actor, S, A1) :-
    % One thing I really don't like about Mercury: This one could be easily
    % implemented with actions_since/3, but then the determinism would be
    % cc_nondet.
    Q = (pred(start(B, _)::in) is semidet :- B = Copycat),
    P = (pred(A::in) is semidet :- ( A = accel(Actor, _) ; A = lc(Actor, _) )),
    find_first_match(P, actions_since(Q, S), A1).

%-----------------------------------------------------------------------------%

:- func accelf(agent, func(rstc.sit(N)) = num(N))
    `with_type` primf(rstc.prim(N)) <= arithmetic.arithmetic(N).
:- mode accelf(in, func(in) = out is det, in) = out is det.
:- mode accelf(in, func(in) = out is semidet, in) = out is det.

accelf(B, AccelF, S) =
    ( if Q = AccelF(S), not infinity(Q) then accel(B, Q) else abort ).


:- func lcf(agent) `with_type` primf(rstc.prim(N)) <= arithmetic.arithmetic(N).

lcf(B, S) = ( if lane(B, S) = left then lc(B, right) else lc(B, left) ).


:- func ntg_after(agent, agent, rstc.sit(N), s(N)) = ntg(N)
    <= arithmetic.arithmetic(N).
:- mode ntg_after(in, in, in, in) = out is semidet.

ntg_after(B, C, S, T) = ntg(B, C, prgolog.do(wait(T), S)).


:- func ttc_after(agent, agent, rstc.sit(N), s(N)) = ttc(N)
    <= arithmetic.arithmetic(N).
:- mode ttc_after(in, in, in, in) = out is semidet.

ttc_after(B, C, S, T) = ttc(B, C, prgolog.do(wait(T), S)).


:- func min_search_time = N <= arithmetic.arithmetic(N).

min_search_time = arithmetic.zero.


:- func max_search_time = N <= arithmetic.arithmetic(N).

max_search_time = arithmetic.from_int(60).


:- func wait_until(func(rstc.sit(N)) = N, N)
    `with_type` primf(rstc.prim(N)) <= arithmetic.arithmetic(N).
:- mode wait_until(in(func(in) = out is semidet), in, in) = out is det.
:- mode wait_until(in(func(in) = out is det), in, in) = out is det.

wait_until(F, Goal, S) = A :-
    if   arithmetic.bin_search(func(T) = F(prgolog.do(wait(number(T)), S)) is semidet,
                               min_search_time, max_search_time, Goal, V1)
    then A = wait(number(V1))
    else A = abort.


:- func basify(func(rstc.sit(N)) = num(N)) = (func(rstc.sit(N)) = N).
:- mode basify(in(func(in) = out is semidet)) =
               out(func(in) = out is semidet) is det.

basify(F) = ( func(S) = X is semidet :- num(X) = F(S) ).

%-----------------------------------------------------------------------------%

follow(B, Victim) = P :-
    P = atomic(
            a(start(B, $pred)) `;`
            t(r((pred(S::in) is semidet :-
                lane(B, S) = lane(Victim, S)
            ))) `;`
            t(r((pred(S::in) is semidet :-
                ntg(B, Victim, S) `in` close_behind
            ))) `;`
            b(accelf(B, rel_v(Victim, B)))
        ) `;`
        a(end(B, $pred)).


tailgate(B, Victim) = P :-
    P = atomic(
            a(start(B, $pred)) `;`
            t(r((pred(S::in) is semidet :-
                lane(B, S) = lane(Victim, S)
            ))) `;`
            t(r((pred(S::in) is semidet :-
                ntg(B, Victim, S) `in_any` [close_behind, very_close_behind]
            ))) `;`
            pickbest(func(X, _Val, _Cmp) = X, one, func(X) =
                a(accel(B, X))
            )
            %b(accelf(B, rel_v(Victim, B)))
        ) `;`
        a(end(B, $pred)).


overtake(B, Victim) = P :-
    P = t(r((pred(S::in) is semidet :-
            lane(B, S) = right,
            lane(Victim, S) = right
        ))) `;`
        t(r((pred(S::in) is semidet :-
            ntg(B, Victim, S) `in` close_behind
        ))) `;`
        b(accelf(B,
            func(S) = number_from_float(1.1) * rel_v(Victim, B, S) is semidet
        )) `;`
        a(lc(B, left)) `;`
        b(wait_until(basify(ntg(B, Victim)), basic(defuzzify(infront)))) `;`
        a(lc(B, right)).


imitate(B, Victim) = P :-
    P = a(start(B, $pred)) `;`
        star(
            b(func(S) = (
                if next_to_be_copied(B, Victim, S, A) then A else abort)
            )
        ) `;`
        a(end(B, $pred)).

%-----------------------------------------------------------------------------%

:- pred poss(prim(N), rstc.sit(N)) <= arithmetic.arithmetic(N).
:- mode poss(in, in) is semidet.
:- mode poss(in(wait), in) is semidet.
:- mode poss(in(accel), in) is semidet.
:- mode poss(in(lc), in) is semidet.
:- mode poss(in(senseD), in) is det.
:- mode poss(in(senseL), in) is det.
:- mode poss(in(init_env), in) is det.
:- mode poss(in(seed), in) is det.
:- mode poss(in(abort), in) is failure.
:- mode poss(in(start), in) is det.
:- mode poss(in(end), in) is det.

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
poss(start(_, _), _).
poss(end(_, _), _).

%-----------------------------------------------------------------------------%

:- func lookahead = lookahead is det.

lookahead = 3.


:- func lookahead(rstc.sit(_)) = lookahead is det.

lookahead(_S) = lookahead.

%-----------------------------------------------------------------------------%

:- func sitlen(rstc.sit(_)) = int.

sitlen(do(_, S)) = 1 + sitlen(S).
sitlen(s0) = 0.


    % Explanation of the reward:
    % Let's assume that an observation consists of only a single action called
    % match(_) and since we want to explain as many observations they give a
    % reward of 1. (In this BAT an observation consists of a wait(_) + match(_),
    % so we could drop the multiplication with 2 from the code, but it doesn't
    % hurt either.)
    % One problem appears though: if all cars maintain their speeds, no actions
    % are needed to explain all incoming observations, and therefore the actual
    % hypothesis program is pulled away. For that reason we have this
    % start(_, _) action which should mark the beginning of the program. Using
    % complex atomic actions, it could enforce execution of one or more actions
    % of the hypothesis program early on.
    % So why not just give start(_, _) a large reward, say, 10? In this case the
    % interpreter might prefer executing one observation, because after that he
    % could still execute the start(_, _) action afterwards. But after the
    % start(_, _) action it needs to execute the real actions of the hypothesis
    % program which don't increase the reward.
    % To avoid this effect, we punish delaying the execution of start(_, _).
    % As the interpreter can not execute more than `lookahead' observations
    % within its lookahead, it prefers the start(_, _) action because it has
    % reward `Bonus * lookahead.' The subtraction of `2 * sitlen(S)' punishes
    % each additional action, particularly observations, by a negative reward
    % of 2 which is greater than the reward of an observation.
    % Note that if S is already a large situation term with
    % Bonus / 2 * lookahead or more actions, this trick obviously doesn't work
    % anymore!
    %
    % Since we are interested in keeping plans like `tailgate' alive as long as
    % possible, we want to defer execution of end(_, _) as long as possible.
    % Therefor we increase the reward with the situation term size by just
    % simply returning sitlen(S).
    % 
:- func reward(rstc.sit(N)) = reward <= arithmetic.arithmetic(N).

reward(s0) = 0.0.
reward(do(A, S)) = bat.reward(S) + New :-
    Bonus = 1000,
    New = (
        if A = start(_, _) then float(max(0, Bonus*lookahead - 2*sitlen(S)))
        else if A = end(_, _) then float(sitlen(S))
        else if A = match(obs(_, D)) then 1.0 - arithmetic.to_float(det_basic(match_dist(D, S)))
        else 0.0
    ).

%-----------------------------------------------------------------------------%

:- pred match_obs(assoc_list(agent, info)::in, rstc.sit(N)::in) is semidet
    <= arithmetic.arithmetic(N).

match_obs(L, S) :-
    all_true((pred(PB::in) is semidet :-
        all_true((pred(PC::in) is semidet :-
            match_info(PB, PC, S)
        ), L)
    ), L),
    all_true((pred(PD::in) is semidet :-
        match_y(PD, S)
    ), L).


:- func match_dist(assoc_list(agent, info), rstc.sit(N)) = num(N)
    <= arithmetic.arithmetic(N).

match_dist(L, S) =
    (   if   Len = 0
        then zero
        else number(Sum `arithmetic.'/'` arithmetic.from_int(Len))
    ) :-
    Zero = {arithmetic.zero, 0},
    Plus = (func({U, V}, {X, Y}) = {U `arithmetic.'+'` X, V `int.'+'` Y}),
    {Sum, Len} = foldl((func(PB, SumLen0) = SumLen0 `Plus`
        foldl((func(PC, SumLen00) =
            SumLen00 `Plus` {match_longitudinal_dist(PC, PB, S), 1}
        ), L, Zero)
    ), L, Zero).


:- pred ntgs(pair(agent, info)::in, pair(agent, info)::in, rstc.sit(N)::in,
             num(N)::out, num(N)::out) is semidet <= arithmetic.arithmetic(N).

ntgs((B - info(VB, _, p(XB, _))), (C - info(_, _, p(XC, _))), S, NTG1, NTG2) :-
    VB \= 0.0,
    NTG1 = ntg(B, C, S),
    NTG2 = number_from_float((XC - XB) / VB) `with_type` num(N).


:- pred ttcs(pair(agent, info)::in, pair(agent, info)::in, rstc.sit(N)::in,
             num(N)::out, num(N)::out) is semidet <= arithmetic.arithmetic(N).

ttcs((B - info(VB, _, p(XB, _))), (C - info(VC, _, p(XC, _))), S, TTC1, TTC2) :-
    VB - VC \= 0.0,
    TTC1 = ttc(B, C, S),
    TTC2 = number_from_float((XC - XB) / (VB - VC)) `with_type` num(N).


:- pred have_common_cat(pred(category), num(N), num(N))
    <= arithmetic.arithmetic(N).
:- mode have_common_cat(pred(out) is multi, in, in) is semidet.

have_common_cat(P, V1, V2) :-
    P(Cat),
    V1 `in` Cat,
    V2 `in` Cat.

    % When two cars have the same speed, TTC is +/-inf.
    % In practice, though, they never have the same speed but continuously
    % balance their speeds. That is, sometimes v(B) >= v(C) and sometimes
    % v(B) =< v(C).
    % This leads to oscillation of TTC: sometimes its +X, then again -X
    % for some really big number X.
    %
    % Problem: our internal model doesn't reflect these oscillations.
    % In fact, it's a weakness of the TTC metric, I guess.
    %
    % To deal with this effect, we allow the model's TTC and the observed
    % TTC to fall into different categories, as long as (= if!) the
    % relative velocity delta does not exceed the value defined by this
    % function:
    %
:- func max_veloc_discrepancy_to_ignore_ttc = float.

max_veloc_discrepancy_to_ignore_ttc = 0.1.


:- pred match_info(pair(agent, info)::in, pair(agent, info)::in,
                   rstc.sit(N)::in) is semidet <= arithmetic.arithmetic(N).

match_info(PB, PC, S) :-
    (   if      some [NTG1, NTG2] ntgs(PB, PC, S, NTG1, NTG2)
        then    have_common_cat((pred(Cat::out) is multi :-
                    ntg_cat(Cat)
                ), NTG1, NTG2)
        else    true
    ),
    (   if      some [TTC1, TTC2] ttcs(PB, PC, S, TTC1, TTC2)
        then    (   have_common_cat((pred(Cat::out) is multi :-
                        ttc_cat(Cat)
                    ), TTC1, TTC2)
                ;   some [NTG1, NTG2] (
                        ntgs(PB, PC, S, NTG1, NTG2),
                        RelV1 = one - NTG1 / TTC1,
                        RelV2 = one - NTG2 / TTC2,
                        Eps = max_veloc_discrepancy_to_ignore_ttc,
                        abs(RelV1 - one) =< number_from_float(Eps),
                        abs(RelV2 - one) =< number_from_float(Eps)
                    )
                )
        else    true
    ).


:- func match_longitudinal_dist(pair(agent, info), pair(agent, info),
                                rstc.sit(N)) = N <= arithmetic.arithmetic(N).

match_longitudinal_dist(PB, PC, S) = D :-
    D1 = (  if      some [NTG1, NTG2] ntgs(PB, PC, S, NTG1, NTG2)
            then    minimize(pred(CatDist::out) is multi :-
                        (
                            CatDist = one
                        ;
                            ntg_cat(Cat),
                            NTG1 `in` Cat,
                            NTG2 `in` Cat,
                            CatDist = abs(NTG1 - NTG2) / max_width(Cat)
                        )
                    )
            else    zero
        ),
    D2 = (  if      some [TTC1, TTC2] ttcs(PB, PC, S, TTC1, TTC2)
            then    minimize(pred(CatDist::out) is multi :-
                        (
                            CatDist = one
                        ;
                            ttc_cat(Cat),
                            TTC1 `in` Cat,
                            TTC2 `in` Cat,
                            (
                                CatDist = abs(TTC1 - TTC2) / max_width(Cat)
                            ;
                                % If they have almost same speed, don't punish
                                % this. See max_veloc_discrepancy_to_ignore_ttc.
                                some [NTG1, NTG2] (
                                    ntgs(PB, PC, S, NTG1, NTG2),
                                    RelV1 = one - NTG1 / TTC1,
                                    RelV2 = one - NTG2 / TTC2,
                                    Eps = max_veloc_discrepancy_to_ignore_ttc,
                                    abs(RelV1 - one) =< number_from_float(Eps),
                                    abs(RelV2 - one) =< number_from_float(Eps)
                                ),
                                CatDist = zero
                            )
                        )
                    )
            else    zero
        ),
    (   if   D3 = D1 + D2, basic(D3)
        then D = det_basic(D3)
        else unexpected($module, string.format("%s: sum not defined: %s, %s",
                                               [s($pred), s(string(D1)),
                                                s(string(D2))]))
    ).


:- pred match_y(pair(agent, info)::in, rstc.sit(_)::in) is semidet.

match_y((B - info(_, _, p(_, Y))), S) :-
    Lane = lane(B, S),
    (   Lane = right, Y < 0.0
    ;   Lane = left,  Y > 0.0
    ).

%-----------------------------------------------------------------------------%

:- pred is_obs_prog(pseudo_atom(prim(N))::in) is semidet
    <= arithmetic.arithmetic(N).

is_obs_prog(complex(seq(pseudo_atom(atom(primf(_))),
                        pseudo_atom(atom(prim(match(_))))))).

%-----------------------------------------------------------------------------%

:- func obs_to_action(obs) = pseudo_atom(prim(N)) <= arithmetic.arithmetic(N).

obs_to_action(Obs @ obs(T, _)) = complex(seq(pseudo_atom(atom(Wait)),
                                             pseudo_atom(atom(Match)))) :-
    T1 = number_from_float(T),
    Wait = primf(
        func(S) = (
            if T0 = start(S), T0 = num(_) then wait(T1 - T0) else abort
        )
    ),
    Match = prim(match(Obs)).

%-----------------------------------------------------------------------------%

:- instance bat(prim(N)) <= arithmetic.arithmetic(N) where [
    pred(poss/2) is bat.poss,
    func(reward/1) is bat.reward,
    func(lookahead/1) is bat.lookahead
].

:- instance obs_bat(prim(N), obs) <= arithmetic.arithmetic(N) where [
    (is_obs_action(match(_))),
    (covered_by_obs(do(A, S)) :-
        (   A \= wait(_), covered_by_obs(S)
        ;   A = match(_) )
    ),
    pred(is_obs_prog/1) is bat.is_obs_prog,
    func(obs_to_action/1) is bat.obs_to_action
].

:- instance pr_bat(prim(N), obs, env) <= arithmetic.arithmetic(N) where [
    seed_init_sit(I) = do(seed(I), s0),
    init_env_sit(env(T, Map), S) = do(init_env(env(T, Map)), S)
].

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.bat.
%-----------------------------------------------------------------------------%

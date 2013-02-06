%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012-2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.debug_bat.m.
% Main author: schwering.
%
% A second BAT the disables the precondition for debugging purposes.
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.debug_bat.

:- interface.

:- use_module util.arithmetic.

%-----------------------------------------------------------------------------%

:- type prim(N) ---> wrapper(rstc.prim(N)).
:- type sit(N) == prgolog.sit(debug_bat.prim(N)).
:- type prog(N) == prgolog.prog(debug_bat.prim(N)).

%-----------------------------------------------------------------------------%

:- func wrap_prog(rstc.prog(N)) = debug_bat.prog(N).
:- func unwrap_prog(debug_bat.prog(N)) = rstc.prog(N).

:- func wrap_sit(rstc.sit(N)) = debug_bat.sit(N).
:- func unwrap_sit(debug_bat.sit(N)) = rstc.sit(N).

:- pred wrap(rstc.prog(N)::in, rstc.sit(N)::in,
             debug_bat.prog(N)::out, debug_bat.sit(N)::out) is det.

:- pred unwrap(debug_bat.prog(N)::in, debug_bat.sit(N)::in,
               rstc.prog(N)::out, rstc.sit(N)::out) is det.

:- pred n_trans(int::in,
                debug_bat.prog(N)::in, debug_bat.sit(N)::in,
                debug_bat.prog(N)::out, debug_bat.sit(N)::out) is semidet
    <= arithmetic.arithmetic(N).

%-----------------------------------------------------------------------------%

:- instance bat(debug_bat.prim(N)) <= arithmetic.arithmetic(N).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module bool.
:- import_module domain.car.rstc.bat.
:- import_module domain.car.rstc.fuzzy.
:- import_module io.
:- import_module list.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

wrap_sit(s0) = s0.
wrap_sit(do(A, S)) = do(wrapper(A), wrap_sit(S)).

unwrap_sit(s0) = s0.
unwrap_sit(do(wrapper(A), S)) = do(A, unwrap_sit(S)).

wrap_prog(seq(P1, P2)) = seq(wrap_prog(P1), wrap_prog(P2)).
wrap_prog(non_det(P1, P2)) = non_det(wrap_prog(P1), wrap_prog(P2)).
wrap_prog(conc(P1, P2)) = conc(wrap_prog(P1), wrap_prog(P2)).
wrap_prog(star(P)) = star(wrap_prog(P)).
wrap_prog(pick(G, X0, P)) = 'new pick'(G, X0, func(X) = wrap_prog(P(X))).
wrap_prog(proc(P)) = proc((func) = wrap_prog(apply(P))).
wrap_prog(pseudo_atom(atom(prim(A)))) = pseudo_atom(atom(prim(wrapper(A)))).
wrap_prog(pseudo_atom(atom(primf(A)))) = pseudo_atom(atom(primf(func(S) = wrapper(A(unwrap_sit(S)))))).
wrap_prog(pseudo_atom(atom(test(T)))) = pseudo_atom(atom(test(func(S) = T(unwrap_sit(S))))).
wrap_prog(pseudo_atom(complex(P))) = pseudo_atom(complex(wrap_prog(P))).
wrap_prog(nil) = nil.

unwrap_prog(seq(P1, P2)) = seq(unwrap_prog(P1), unwrap_prog(P2)).
unwrap_prog(non_det(P1, P2)) = non_det(unwrap_prog(P1), unwrap_prog(P2)).
unwrap_prog(conc(P1, P2)) = conc(unwrap_prog(P1), unwrap_prog(P2)).
unwrap_prog(star(P)) = star(unwrap_prog(P)).
unwrap_prog(pick(G, X0, P)) = 'new pick'(G, X0, func(X) = unwrap_prog(P(X))).
unwrap_prog(proc(P)) = proc((func) = unwrap_prog(apply(P))).
unwrap_prog(pseudo_atom(atom(prim(wrapper(A))))) = pseudo_atom(atom(prim(A))).
unwrap_prog(pseudo_atom(atom(primf(A)))) = pseudo_atom(atom(primf((func(S) = A1 is det :- wrapper(A1) = A(wrap_sit(S)))))).
unwrap_prog(pseudo_atom(atom(test(T)))) = pseudo_atom(atom(test(func(S) = T(wrap_sit(S))))).
unwrap_prog(pseudo_atom(complex(P))) = pseudo_atom(complex(unwrap_prog(P))).
unwrap_prog(nil) = nil.

wrap(P, S, wrap_prog(P), wrap_sit(S)).

unwrap(P, S, unwrap_prog(P), unwrap_sit(S)).

%-----------------------------------------------------------------------------%

n_trans(I, P, S, P2, S2) :-
    if      I = 0
    then    P2 = P, S2 = S
    else    prgolog.trans(P, S, P1, S1),
            n_trans(I-1, P1, S1, P2, S2).

%-----------------------------------------------------------------------------%

:- pred poss(rstc.prim(N), rstc.sit(N)) <= arithmetic.arithmetic(N).
:- mode poss(in, in) is semidet.

poss(wait(T), S) :-
    trace [io(!IO)] ( format("poss(wait(%s))\n", [s(string(T))], !IO) ),
    check_time(T, S).
poss(accel(_, _), _).
poss(lc(_, _), _).
poss(senseD(_, _, _, _), _).
poss(senseL(_, _), _).
poss(init_env(_), _).
poss(match(Obs), S) :-
    trace [io(!IO)] ( format("poss(match(%s, ...))\n", [s(string(time(Obs)))], !IO) ),
    check_obs(Obs, S).
poss(seed(_), _).
poss(abort, _) :-
    trace [io(!IO)] ( format("poss(abort), ignoring\n", [], !IO) ),
    true. % !!!
poss(noop, _).
poss(start(_, _), _).
poss(end(_, _), _).


%-----------------------------------------------------------------------------%

:- func lookahead(rstc.sit(N)) = lookahead is det <= arithmetic.arithmetic(N).

lookahead(_) = 0.


:- func new_lookahead(lookahead, rstc.sit(N)) = lookahead is det
    <= arithmetic.arithmetic(N).

new_lookahead(L, _) = L - 1.

%-----------------------------------------------------------------------------%

:- func reward(rstc.prim(N), rstc.sit(N)) = reward <= arithmetic.arithmetic(N).

reward(A, _) = New :-
    New = (
        if      A = end(_, _)
        then    0.0% -1.0% float(2 * sitlen(S))
        else if A = match(_)
        then    2.0
        else    0.0
    ).


:- func reward_bound(atom(debug_bat.prim(_))) = reward.

reward_bound(_) = 2.0.

%-----------------------------------------------------------------------------%

:- pred check_time(s(N)::in, rstc.sit(N)::in) is semidet
    <= arithmetic.arithmetic(N).

check_time(T, _S) :-
    if      T >= zero
    then    true
    else    trace [io(!IO)] (
                format("wait time is %s\n", [s(string(T))], !IO)
            ),
            fail.

%-----------------------------------------------------------------------------%

:- pred check_obs(Obs::in, rstc.sit(N)::in) is semidet
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

check_obs(Obs, S) :-
    and_list(map((func({B, C}) =
        check_info(Obs, B, C, S)
    ), agent_pairs)) = B1,
    and_list(map((func(D) = check_y(Obs, D, S)), agents)) = B2,
    and(B1, B2) = yes.


:- func check_dist(Obs, rstc.sit(N)) = num(N)
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

check_dist(Obs, S) =
    (   if   Len = 0
        then zero
        else number(Sum `arithmetic.'/'` arithmetic.from_int(Len))
    ) :-
    %Zero = {arithmetic.zero, 0},
    %Plus = (func({U, V}, {X, Y}) = {U `arithmetic.'+'` X, V `int.'+'` Y}),
    Sum = foldl((func({B, C}, Sum0) = Sum0 `arithmetic.'+'`
        check_longitudinal_dist(Obs, C, B, S)
    ), agent_pairs, arithmetic.zero),
    Len = length(agent_pairs).


:- pred ntgs(Obs::in, agent::in, agent::in, rstc.sit(N)::in,
             num(N)::out, num(N)::out) is semidet
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

ntgs(Obs, B, C, S, ntg(B, C, S), number_from_float(net_time_gap(Obs, B, C))).


:- pred ttcs(Obs::in, agent::in, agent::in, rstc.sit(N)::in,
             num(N)::out, num(N)::out) is semidet
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

ttcs(Obs, B, C, S, ttc(B, C, S), number_from_float(time_to_collision(Obs, B, C))).


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


:- func check_info(Obs, agent, agent, rstc.sit(N)) = bool
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

check_info(Obs, B, C, S) = and(Outcome1, Outcome2) :-
    (   if      some [NTG1, NTG2] ntgs(Obs, B, C, S, NTG1, NTG2)
        then    Outcome1 = pred_to_bool((pred) is semidet :-
                    (
                        have_common_cat((pred(Cat::out) is multi :-
                            ntg_cat(Cat)
                        ), NTG1, NTG2)
                    ->
                        true
                    ;   trace [io(!IO)] (
                            format("no category for ntg(%s, %s): %s %s\n", [s(string(B)), s(string(C)), s(string(NTG1)), s(string(NTG2))], !IO)
                        ),
                        false
                    )
                )
        else    Outcome1 = yes
    ),
    (   if      some [TTC1, TTC2] ttcs(Obs, B, C, S, TTC1, TTC2)
        then    Outcome2 = pred_to_bool((pred) is semidet :-
                    (
                        have_common_cat((pred(Cat::out) is multi :- ttc_cat(Cat)), TTC1, TTC2)
                    ->
                        true
                    ;   trace [io(!IO)] ( format("no category ttc(%s, %s): %s %s\n", [s(string(B)), s(string(C)), s(string(TTC1)), s(string(TTC2))], !IO) ),
                        false
                    ;   some [NTG1, NTG2] (
                            ntgs(Obs, B, C, S, NTG1, NTG2),
                            RelV1 = one - NTG1 / TTC1,
                            RelV2 = one - NTG2 / TTC2,
                            Eps = max_veloc_discrepancy_to_ignore_ttc,
                            abs(RelV1 - one) =< number_from_float(Eps),
                            abs(RelV2 - one) =< number_from_float(Eps)
                        ) -> true ; trace [io(!IO)] ( format("no blabla(%s, %s): %s %s\n", [s(string(B)), s(string(C)), s(string(TTC1)), s(string(TTC2))], !IO) ), false
                    )
                )
        else    Outcome2 = yes
    ).


:- func check_longitudinal_dist(Obs, agent, agent, rstc.sit(N)) = N
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

check_longitudinal_dist(Obs, B, C, S) = D :-
    D1 = (  if      some [NTG1, NTG2] ntgs(Obs, B, C, S, NTG1, NTG2)
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
    D2 = (  if      some [TTC1, TTC2] ttcs(Obs, B, C, S, TTC1, TTC2)
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
                                    ntgs(Obs, B, C, S, NTG1, NTG2),
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
    (   if   D3 = (D1 + D2) / (one + one), basic(D3)
        then D = det_basic(D3)
        else unexpected($module, string.format("%s: sum not defined: %s, %s",
                                               [s($pred), s(string(D1)),
                                                s(string(D2))]))
    ).


:- func check_y(Obs, agent, rstc.sit(_)) = bool <= car_obs(Obs).

check_y(Obs, B, S) = Outcome :-
    Outcome = pred_to_bool((pred) is semidet :-
        if      Y = y_pos(Obs, B)
        then    ( Y =< 0.0, lane(B, S) = right
                ; Y >= 0.0, lane(B, S) = left
            %%% ; trace [io(!IO)] ( format("Y = %f, lane = %s\n", [f(Y), s(string(lane(B, S)))], !IO) ), fail
                )
        else    true
    ).

%-----------------------------------------------------------------------------%

:- instance bat(debug_bat.prim(N)) <= arithmetic.arithmetic(N) where [
    (poss(wrapper(A), S) :- debug_bat.poss(A, unwrap_sit(S))),
    (reward_bound(A) = debug_bat.reward_bound(A)),
    (reward(wrapper(A), S) = debug_bat.reward(A, unwrap_sit(S))),
    (lookahead(S) = debug_bat.lookahead(unwrap_sit(S)))
].

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.debug_bat.
%-----------------------------------------------------------------------------%

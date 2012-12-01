%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.m.
% Main author: schwering.
%
% Basic action theory (BAT) for driving with two simple actions, set_yaw and
% set_veloc that control the steering and speed of the vehicle.
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.

:- interface.

:- import_module arithmetic.
:- import_module prgolog.
:- import_module prgolog.nice.

%-----------------------------------------------------------------------------%

:- type s(N) == N.
:- type ntg(N) == s(N).
:- type ttc(N) == s(N).

:- type prim(N)
    --->    wait(s(N))
    ;       accel(agent, N)
    ;       lc(agent, lane)
    ;       senseD(agent, agent, ntg(N), ttc(N))
    ;       senseL(agent, lane)
    ;       init_env(env)
    ;       match(obs)
    ;       seed(int)
    ;       abort.
:- inst wait ---> wait(ground).
:- inst accel ---> accel(ground, ground).
:- inst lc ---> lc(ground, ground).
:- inst senseD ---> senseD(ground, ground, ground, ground).
:- inst senseL ---> senseL(ground, ground).
:- inst init_env ---> init_env(ground).
:- inst match ---> match(ground).
:- inst seed ---> seed(ground).
:- inst abort ---> abort.

:- type sit(N) == prgolog.sit(prim(N)).
:- type prog(N) == prgolog.prog(prim(N)).
:- type proc(N) == prgolog.proc(prim(N)).
:- type conf(N) == prgolog.nice.conf(prim(N)).

%-----------------------------------------------------------------------------%

:- func ntg(agent, agent, rstc.sit(N)) = ntg(N) <= arithmetic(N).
:- mode ntg(in, in, in) = out is semidet.

:- func ttc(agent, agent, rstc.sit(N)) = ttc(N) <= arithmetic(N).
:- mode ttc(in, in, in) = out is semidet.

:- func lane(agent, rstc.sit(N)) = lane.

:- func start(rstc.sit(N)) = s(N) <= arithmetic(N).

%-----------------------------------------------------------------------------%

:- func rel_v(agent, agent, rstc.sit(N)) = N <= arithmetic(N).
:- mode rel_v(in, in, in) = out is semidet.

:- pred opposite_direction(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic(N).

:- pred same_direction(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic(N).

:- pred slower(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic(N).

:- pred faster(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic(N).

%-----------------------------------------------------------------------------%

:- include_module bat.
:- include_module fuzzy.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module math.
:- import_module prgolog.nice.
:- import_module solutions.

%-----------------------------------------------------------------------------%

:- func N // N = N <= arithmetic(N).
:- mode in // in = out is semidet.

V // W = R :- W \= zero, R = unchecked_quotient(V, W).

%-----------------------------------------------------------------------------%

:- func min({N, agent}, {N, agent}) = ({N, agent}) <= arithmetic(N).

min(E1 @ {Cost1, _}, E2 @ {Cost2, _}) = ( if Cost1 < Cost2 then E1 else E2 ).


:- func transitive_car(agent::in, agent::in, rstc.sit(N)::in) = (agent::out)
    is semidet <= arithmetic(N).

transitive_car(B, D, S) = E :-
    Cars = solutions((pred(C::out) is nondet :- agent(C), C \= B, C \= D)),
    map((pred(C::in, {Cost, C}::out) is semidet :-
        ttc(B, C, S) \= zero,
        ttc(C, D, S) \= zero,
        Cost = abs(ntg(B, C, S) + ntg(C, D, S) + ttc(B, C, S) + ttc(C, D, S))
    ), Cars, [FirstCandidate | Candidates]),
    {_, E} = foldr(min, Candidates, FirstCandidate).

%-----------------------------------------------------------------------------%

ntg(B, D, do(A, S)) = R :-
    B \= D,
    promise_equivalent_solutions [R]
    (
        A = wait(T),
        (   if
                ttc(B, D, S) \= zero
            then
                R = ntg(B, D, S) - T * ntg(B, D, S) // ttc(B, D, S)
            else
                C = transitive_car(B, D, S),
                R1 = ntg(B, C, S) - T * ntg(B, C, S) // ttc(B, C, S),
                R2 = ttc(B, C, S) - T,
                R3 = ntg(C, D, S) - T * ntg(C, D, S) // ttc(C, D, S),
                R = R1 + (one - R1 // R2) * R3
        )
    ;
        A = accel(B, Q),
        R = one // Q * ntg(B, D, S)
    ;
        A = senseD(B, D, NTG_BD, _),
        R = NTG_BD
    ;
        A = senseD(D, B, NTG_DB, TTC_DB),
        R = -one // (one - NTG_DB // TTC_DB) * NTG_DB
    ;
        A = init_env(Env),
        R = ntg_from_env(B, D, Env)
    ;
        A \= wait(_),
        A \= accel(B, _),
        A \= senseD(B, D, _, _),
        A \= senseD(D, B, _, _),
        A \= init_env(_),
        R = ntg(B, D, S)
    ).

%-----------------------------------------------------------------------------%

ttc(B, D, do(A, S)) = R :-
    B \= D,
    promise_equivalent_solutions [R]
    (
        A = wait(T),
        R = ttc(B, D, S) - T
    ;
        A = accel(B, Q),
        (   if
                ntg(B, D, S) \= zero,
                ttc(B, D, S) \= zero
            then
                Q \= one - ntg(B, D, S) // ttc(B, D, S),
                R = one // ((Q - one) * ttc(B, D, S) // ntg(B, D, S) + one)
                    * ttc(B, D, S)
            else
                C = transitive_car(B, D, S),
                Q \= one - ntg(B, C, S) // ttc(B, C, S),
                TTC_BC = one // ((Q - one) * ttc(B, C, S) // ntg(B, C, S) + one)
                    * ttc(B, C, S),
                R1 = (ttc(C, D, S) * ntg(B, C, S))
                    // (ntg(C, D, S) * TTC_BC + ttc(C, D, S) * ntg(B, C, S)
                        - ntg(C, D, S) * ntg(B, C, S)),
                R2 = (TTC_BC * ntg(C, D, S) - ntg(B, C, S) * ntg(C, D, S))
                    // (ntg(B, C, S) * ttc(C, D, S) + TTC_BC * ntg(C, D, S)
                        - ntg(B, C, S) * ntg(C, D, S)),
                R = R1 * TTC_BC + R2 * ttc(C, D, S)
        )
    ;
        A = accel(D, Q),
        (   if
                ntg(B, D, S) \= zero,
                ttc(B, D, S) \= zero
            then
                Q \= one // (one - ntg(B, D, S) // ttc(B, D, S)),
                R = one // ((one - Q) * ttc(B, D, S) // ntg(B, D, S) + Q)
                    * ttc(B, D, S)
            else
                C = transitive_car(B, D, S),
                Q \= one // (one - ntg(C, D, S) // ttc(C, D, S)),
                NTG_CD = one // Q * ntg(C, D, S),
                TTC_CD = one // ((one - Q) * ttc(C, D, S) // ntg(C, D, S) + Q)
                    * ttc(C, D, S),
                R1 = (TTC_CD * ntg(B, C, S))
                    // (NTG_CD * ttc(B, C, S) + TTC_CD * ntg(B, C, S)
                        - NTG_CD * ntg(B, C, S)),
                R2 = (ttc(B, C, S) * NTG_CD - ntg(B, C, S) * NTG_CD)
                    // (ntg(B, C, S) * TTC_CD + ttc(B, C, S) * NTG_CD
                        - ntg(B, C, S) * NTG_CD),
                R = R1 * ttc(B, C, S) + R2 * TTC_CD
        )
    ;
        A = senseD(B, D, _, TTC_BD),
        R = TTC_BD
    ;
        A = senseD(D, B, _, TTC_DB),
        R = TTC_DB
    ;
        A = init_env(Env),
        R = ttc_from_env(B, D, Env)
    ;
        A \= wait(_),
        A \= accel(B, _),
        A \= accel(D, _),
        A \= senseD(B, D, _, _),
        A \= senseD(D, B, _, _),
        A \= init_env(_),
        R = ttc(B, D, S)
    ).

%-----------------------------------------------------------------------------%

lane(_, s0) = right.
lane(B, do(A, S)) = L :-
    if      A = init_env(Env)
    then    L = lane_from_env(B, Env)
    else if A = lc(B, L0)
    then    L = L0
    else    L = lane(B, S).

%-----------------------------------------------------------------------------%

start(s0) = zero.
start(do(A, S)) = T :-
    if      A = init_env(Env)
    then    T = from_float(start_from_env(Env))
    else if A = wait(D)
    then    T = start(S) + D
    else    T = start(S).
%-----------------------------------------------------------------------------%

:- func ntg_from_env(agent, agent, env) = ntg(N) <= arithmetic(N).
:- mode ntg_from_env(in, in, in) = out is semidet.

ntg_from_env(B, D, env(_, Map)) = R :-
    info(FVB, _, PosB) = Map^det_elem(B),
    info(_, _, PosD) = Map^det_elem(D),
    XB = from_float(x(PosB)),
    XD = from_float(x(PosD)),
    VB = from_float(FVB),
    R = (XD - XB) // VB.


:- func ttc_from_env(agent, agent, env) = ttc(N) <= arithmetic(N).
:- mode ttc_from_env(in, in, in) = out is semidet.

ttc_from_env(B, D, env(_, Map)) = R :-
    info(FVB, _, PosB) = Map^det_elem(B),
    info(FVD, _, PosD) = Map^det_elem(D),
    XB = from_float(x(PosB)),
    XD = from_float(x(PosD)),
    VB = from_float(FVB),
    VD = from_float(FVD),
    R = (XD - XB) // (VB - VD).


:- func lane_from_env(agent, env) = lane is det.

lane_from_env(B, env(_, Map)) = ( if Y `float.'<'` 0.0 then right else left ) :-
    info(_, _, p(_, Y)) = Map^det_elem(B).


:- func start_from_env(env) = s is det.

start_from_env(env(T, _)) = T.

%-----------------------------------------------------------------------------%

rel_v(C, B, S) = one - ntg(B, C, S) // ttc(B, C, S).

opposite_direction(B, C, S) :- rel_v(B, C, S) > zero.

same_direction(B, C, S) :- rel_v(B, C, S) < zero.

slower(B, C, S) :- rel_v(B, C, S) > -one, rel_v(B, C, S) < one.

faster(B, C, S) :- not slower(B, C, S).

%-----------------------------------------------------------------------------%

:- pred is_match_action(prim(N)::in) is semidet.

is_match_action(match(_)).


:- func last_match(rstc.sit(N)) = prim(N) is semidet.

last_match(do(A, S)) = ( if is_match_action(A) then A else last_match(S) ).

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.
%-----------------------------------------------------------------------------%

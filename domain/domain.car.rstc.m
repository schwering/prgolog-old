%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.m.
% Main author: schwering.
%
% The foundational part of a basic action theory (BAT) for driving with three
% core actions: wait to induce time lapse, accel to increase or decrease speed
% relatively, and lc to change the lane.
%
% This module defines the partial functions for net time gap (NTG) and time to
% collision (TTC). Note that they are transitive and symmetric in the sense
% that if we know NTG and TTC for the pairs A, B and B, C, we can infer the
% NTG and TTC of the pair A, C. For details check the RSTC documentation
% (unpublished).
%
% Further fluents and the rest for the BAT typeclass instance are defined in the
% rstc.bat submodule.
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.

:- interface.

:- use_module arithmetic.
:- import_module inf_arithmetic.
:- import_module prgolog.
:- import_module prgolog.nice.

%-----------------------------------------------------------------------------%

:- type basic_s(N) == N.
:- type s(N) == num(N).
:- type ntg(N) == s(N).
:- type ttc(N) == s(N).
:- type scale(N) == num(N).

:- type prim(N)
    --->    wait(s(N))
    ;       accel(agent, scale(N))
    ;       lc(agent, lane)
    ;       senseD(agent, agent, ntg(N), ttc(N))
    ;       senseL(agent, lane)
    ;       init_env(env)
    ;       match(obs)
    ;       seed(int)
    ;       abort
    ;       start(agent, string)
    ;       end(agent, string).

:- inst wait ---> wait(ground).
:- inst accel ---> accel(ground, ground).
:- inst lc ---> lc(ground, ground).
:- inst senseD ---> senseD(ground, ground, ground, ground).
:- inst senseL ---> senseL(ground, ground).
:- inst init_env ---> init_env(ground).
:- inst match ---> match(ground).
:- inst seed ---> seed(ground).
:- inst abort ---> abort.
:- inst start ---> start(ground, ground).
:- inst end ---> end(ground, ground).

:- inst finite_time_action
    --->    wait(basic_num)
    ;       accel(ground, basic_num) % XXX really?
    ;       lc(ground, ground)
    ;       senseD(ground, ground, ground, ground)
    ;       senseL(ground, ground)
    ;       init_env(ground)
    ;       match(ground)
    ;       seed(ground)
    ;       abort
    ;       start(ground, ground)
    ;       end(ground, ground).

:- inst finite_time_sit ---> s0 ; do(finite_time_action, finite_time_sit).

:- type sit(N) == prgolog.sit(prim(N)).
:- type prog(N) == prgolog.prog(prim(N)).
:- type proc(N) == prgolog.proc(prim(N)).
:- type conf(N) == prgolog.nice.conf(prim(N)).

%-----------------------------------------------------------------------------%

:- func ntg(agent, agent, rstc.sit(N)) = ntg(N) <= arithmetic.arithmetic(N).
:- mode ntg(in, in, in) = out is semidet.

:- func ttc(agent, agent, rstc.sit(N)) = ttc(N) <= arithmetic.arithmetic(N).
:- mode ttc(in, in, in) = out is semidet.

:- func lane(agent, rstc.sit(N)) = lane.

:- func start(rstc.sit(N)) = s(N) <= arithmetic.arithmetic(N).
:- mode start(in(finite_time_sit)) = out(basic_num) is det.
:- mode start(in) = out is semidet.

%-----------------------------------------------------------------------------%

:- use_module io.

:- pred print_stats(io.io::di, io.io::uo) is det.
:- pred reset(io.io::di, io.io::uo) is det.

%-----------------------------------------------------------------------------%

:- include_module bat.
:- include_module fuzzy.
:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module math.
:- import_module prgolog.nice.
:- import_module solutions.

%-----------------------------------------------------------------------------%

:- pragma memo(ntg/3, [allow_reset, statistics, fast_loose]). 
:- pragma memo(ttc/3, [allow_reset, statistics, fast_loose]). 
:- pragma memo(lane/2, [allow_reset, statistics, fast_loose]). 

%-----------------------------------------------------------------------------%

:- func min({num(N), T}, {num(N), T}) = ({num(N), T})
    <= arithmetic.arithmetic(N).

min(E1 @ {Cost1, _}, E2 @ {Cost2, _}) = ( if Cost1 < Cost2 then E1 else E2 ).


:- func transitive_car(agent::in, agent::in, rstc.sit(N)::in) = (agent::out)
    is semidet <= arithmetic.arithmetic(N).

transitive_car(B, D, S) = E :-
    Cars = solutions((pred(C::out) is nondet :- agent(C), C \= B, C \= D)),
    filter_map((pred(C::in, {Cost, C}::out) is semidet :-
        ttc(B, C, S) \= zero,
        ttc(C, D, S) \= zero,
        Cost = abs(ntg(B, C, S)) + abs(ntg(C, D, S)) +
               abs(ttc(B, C, S)) + abs(ttc(C, D, S))
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
                R = ntg(B, D, S) - T * ntg(B, D, S) / ttc(B, D, S)
            else
                C = transitive_car(B, D, S),
                R1 = ntg(B, C, S) - T * ntg(B, C, S) / ttc(B, C, S),
                R2 = ttc(B, C, S) - T,
                R3 = ntg(C, D, S) - T * ntg(C, D, S) / ttc(C, D, S),
                R = R1 + (one - R1 / R2) * R3
        )
    ;
        A = accel(B, Q),
        R = one / Q * ntg(B, D, S)
    ;
        A = senseD(B, D, NTG_BD, _),
        R = NTG_BD
    ;
        A = senseD(D, B, NTG_DB, TTC_DB),
        R = -one / (one - NTG_DB / TTC_DB) * NTG_DB
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
                %Q \= one - ntg(B, D, S) / ttc(B, D, S),
                R = one / ((Q - one) * ttc(B, D, S) / ntg(B, D, S) + one)
                    * ttc(B, D, S)
            else
                C = transitive_car(B, D, S),
                %Q \= one - ntg(B, C, S) / ttc(B, C, S),
                TTC_BC = one / ((Q - one) * ttc(B, C, S) / ntg(B, C, S) + one)
                    * ttc(B, C, S),
                R1 = (ttc(C, D, S) * ntg(B, C, S))
                    / (ntg(C, D, S) * TTC_BC + ttc(C, D, S) * ntg(B, C, S)
                        - ntg(C, D, S) * ntg(B, C, S)),
                R2 = (TTC_BC * ntg(C, D, S) - ntg(B, C, S) * ntg(C, D, S))
                    / (ntg(B, C, S) * ttc(C, D, S) + TTC_BC * ntg(C, D, S)
                        - ntg(B, C, S) * ntg(C, D, S)),
                R = R1 * TTC_BC + R2 * ttc(C, D, S)
        )
    ;
        A = accel(D, Q),
        (   if
                ntg(B, D, S) \= zero,
                ttc(B, D, S) \= zero
            then
                %Q \= one / (one - ntg(B, D, S) / ttc(B, D, S)),
                R = one / ((one - Q) * ttc(B, D, S) / ntg(B, D, S) + Q)
                    * ttc(B, D, S)
            else
                C = transitive_car(B, D, S),
                %Q \= one / (one - ntg(C, D, S) / ttc(C, D, S)),
                NTG_CD = one / Q * ntg(C, D, S),
                TTC_CD = one / ((one - Q) * ttc(C, D, S) / ntg(C, D, S) + Q)
                    * ttc(C, D, S),
                R1 = (TTC_CD * ntg(B, C, S))
                    / (NTG_CD * ttc(B, C, S) + TTC_CD * ntg(B, C, S)
                        - NTG_CD * ntg(B, C, S)),
                R2 = (ttc(B, C, S) * NTG_CD - ntg(B, C, S) * NTG_CD)
                    / (ntg(B, C, S) * TTC_CD + ttc(B, C, S) * NTG_CD
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
    then    T = number_from_float(start_from_env(Env))
    else if A = wait(D)
    then    T = start(S) + D
    else    T = start(S).

%-----------------------------------------------------------------------------%

:- func ntg_from_env(agent, agent, env) = ntg(N) <= arithmetic.arithmetic(N).
:- mode ntg_from_env(in, in, in) = out is semidet.

ntg_from_env(B, D, env(_, Map)) = R :-
    info(FVB, _, PosB) = Map^det_elem(B),
    info(_, _, PosD) = Map^det_elem(D),
    XB = number_from_float(x(PosB)),
    XD = number_from_float(x(PosD)),
    VB = number_from_float(FVB),
    R = (XD - XB) / VB.


:- func ttc_from_env(agent, agent, env) = ttc(N) <= arithmetic.arithmetic(N).
:- mode ttc_from_env(in, in, in) = out is semidet.

ttc_from_env(B, D, env(_, Map)) = R :-
    info(FVB, _, PosB) = Map^det_elem(B),
    info(FVD, _, PosD) = Map^det_elem(D),
    XB = number_from_float(x(PosB)),
    XD = number_from_float(x(PosD)),
    VB = number_from_float(FVB),
    VD = number_from_float(FVD),
    R = (XD - XB) / (VB - VD).


:- func lane_from_env(agent, env) = lane is det.

lane_from_env(B, env(_, Map)) = ( if Y `float.'<'` 0.0 then right else left ) :-
    info(_, _, p(_, Y)) = Map^det_elem(B).


:- func start_from_env(env) = s is det.

start_from_env(env(T, _)) = T.

%-----------------------------------------------------------------------------%

:- pred is_match_action(prim(N)::in) is semidet.

is_match_action(match(_)).


:- func last_match(rstc.sit(N)) = prim(N) is semidet.

last_match(do(A, S)) = ( if is_match_action(A) then A else last_match(S) ).

%-----------------------------------------------------------------------------%

:- import_module table_statistics.

print_stats(!IO) :-
    domain.car.rstc.table_statistics_for_ntg_3(NTG, !IO),
    io.write_string("\nntg/3:\n", !IO),
    write_table_stats(current_stats(call_table_stats(NTG)), !IO),
    %
    domain.car.rstc.table_statistics_for_ttc_3(TTC, !IO),
    io.write_string("\nttc/3:\n", !IO),
    write_table_stats(current_stats(call_table_stats(TTC)), !IO),
    %
    domain.car.rstc.table_statistics_for_lane_2(Lane, !IO),
    io.write_string("\nlane/2:\n", !IO),
    write_table_stats(current_stats(call_table_stats(Lane)), !IO).


reset(!IO) :-
    table_reset_for_ntg_3(!IO),
    table_reset_for_ttc_3(!IO),
    table_reset_for_lane_2(!IO).

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.
%-----------------------------------------------------------------------------%

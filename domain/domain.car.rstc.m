%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012-2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
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

:- import_module prgolog.
:- import_module prgolog.nice.
:- import_module util.
:- use_module util.arithmetic.
:- import_module util.inf_arithmetic.

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
    ;       init_env(ext_env(N))
    ;       match(obs)
    ;       seed(int)
    ;       abort
    ;       noop
    ;       start(agent, string)
    ;       end(agent, string).

:- type ext_env(N)
    --->    abs_env(env)        % for situation initialization from simulator
    ;       rel_env(s(N),       % for progression; entries for all fluents
                    assoc_list({agent, agent}, ntg(N)),
                    assoc_list({agent, agent}, ttc(N)),
                    assoc_list(agent, lane),
                    reward,
                    int).

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
    ;       init_env(finite_time_ext_env)
    ;       match(ground)
    ;       seed(ground)
    ;       abort
    ;       start(ground, ground)
    ;       end(ground, ground).

:- inst finite_time_ext_env
    --->    abs_env(ground)
    ;       rel_env(basic_num, ground, ground, ground, ground, ground).

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

:- pred print_memo_stats(io.io::di, io.io::uo) is det.
:- pred reset_memo(io.io::di, io.io::uo) is det.

%-----------------------------------------------------------------------------%

:- include_module bat.
:- include_module debug_bat.
:- include_module fuzzy.
:- include_module progression.
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
    if      A = init_env(Env), L0 = lane_from_env(B, Env)
    then    L = L0
    else if A = lc(B, L0)
    then    L = L0
    else    L = lane(B, S).

%-----------------------------------------------------------------------------%

start(s0) = zero.
start(do(A, S)) = T :-
    if      A = init_env(Env)
    then    T = start_from_env(Env)
    else if A = wait(D)
    then    T = start(S) + D
    else    T = start(S).

%-----------------------------------------------------------------------------%

:- func ntg_from_env(agent, agent, ext_env(N)) = ntg(N)
    <= arithmetic.arithmetic(N).
:- mode ntg_from_env(in, in, in) = out is semidet.

ntg_from_env(B, D, abs_env(env(_, Map))) = R :-
    info(FVB, _, PosB) = Map^elem(B),
    info(_, _, PosD) = Map^elem(D),
    XB = number_from_float(x(PosB)),
    XD = number_from_float(x(PosD)),
    VB = number_from_float(FVB),
    R = (XD - XB) / VB.
ntg_from_env(B, D, rel_env(_, NTGs, _, _, _, _)) = R :-
    search(NTGs, {B, D}, R).


:- func ttc_from_env(agent, agent, ext_env(N)) = ttc(N)
    <= arithmetic.arithmetic(N).
:- mode ttc_from_env(in, in, in) = out is semidet.

ttc_from_env(B, D, abs_env(env(_, Map))) = R :-
    info(FVB, _, PosB) = Map^elem(B),
    info(FVD, _, PosD) = Map^elem(D),
    XB = number_from_float(x(PosB)),
    XD = number_from_float(x(PosD)),
    VB = number_from_float(FVB),
    VD = number_from_float(FVD),
    R = (XD - XB) / (VB - VD).
ttc_from_env(B, D, rel_env(_, _, TTCs, _, _, _)) = R :-
    search(TTCs, {B, D}, R).


:- func lane_from_env(agent, ext_env(N)) = lane is semidet.

lane_from_env(B, abs_env(env(_, Map))) = L0 :-
    info(_, _, p(_, Y)) = Map^elem(B),
    L0 = ( if Y `float.'<'` 0.0 then right else left ).
lane_from_env(B, rel_env(_, _, _, Lanes, _, _)) = L0 :-
    search(Lanes, B, L0).


:- func start_from_env(ext_env(N)) = s(N) is det <= arithmetic.arithmetic(N).

start_from_env(abs_env(env(T, _))) = number_from_float(T).
start_from_env(rel_env(T, _, _, _, _, _)) = T.

%-----------------------------------------------------------------------------%

:- pred is_match_action(prim(N)::in) is semidet.

is_match_action(match(_)).


:- func last_match(rstc.sit(N)) = prim(N) is semidet.

last_match(do(A, S)) = ( if is_match_action(A) then A else last_match(S) ).

%-----------------------------------------------------------------------------%

:- import_module table_statistics.

print_memo_stats(!IO) :-
    table_statistics_for_ntg_3(NTG, !IO),
    io.write_string("\nntg/3:\n", !IO),
    write_table_stats(current_stats(call_table_stats(NTG)), !IO),
    %
    table_statistics_for_ttc_3(TTC, !IO),
    io.write_string("\nttc/3:\n", !IO),
    write_table_stats(current_stats(call_table_stats(TTC)), !IO),
    %
    table_statistics_for_lane_2(Lane, !IO),
    io.write_string("\nlane/2:\n", !IO),
    write_table_stats(current_stats(call_table_stats(Lane)), !IO),
    true.


reset_memo(!IO) :-
    table_reset_for_ntg_3(!IO),
    table_reset_for_ttc_3(!IO),
    table_reset_for_lane_2(!IO),
    true.

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.
%-----------------------------------------------------------------------------%

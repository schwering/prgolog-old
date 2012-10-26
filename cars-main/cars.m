%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: cars.m.
% Main author: schwering.
%
% Main predicate that takes observations from stdin and performs plan
% recognition.
%
%-----------------------------------------------------------------------------%

:- module cars.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module domain.
:- import_module domain.car.
:- import_module domain.car.cont.
:- import_module domain.car.cont.io_util.
:- import_module domain.car.obs.
:- import_module domain.car.obs.stdin.
:- import_module int.
:- import_module float.
:- import_module list.
:- import_module planrecog.
:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module prgolog.nice.
:- import_module string.
:- import_module times.
%:- import_module table_statistics.

%-----------------------------------------------------------------------------%

:- import_module thread.

:- pred forward_obs(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    forward_obs(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    read_obs();
    IO1 = IO0;
").

main(!IO) :-
    times(Tms2, !IO),
    Source = source,
    Prog = (p(cruise(b)) // p(overtake(c, b))) `with_type` prog(prim, stoch, proc),
    %spawn((pred(IO0::di, IO1::uo) is cc_multi :- forward_obs(IO0, IO1)), !IO),
    %planrecog(10, global_init_obs, global_next_obs, Prog, Results, !IO),
    %online_planrecog(10, Vars, !IO),
    %wait_for_planrecog_finish(Vars, !IO),
    %Results = [],
    planrecog(10, Source, Prog, Results, !IO),
    times(Tms3, !IO),
    map0_io((pred(s_state(conf(P, S), R)::in, IO0::di, IO1::uo) is det :-
        some [!SubIO] (
            IO0 = !:SubIO,
            write(R, !SubIO), nl(!SubIO),
            (   if      solve(vargen(S), constraints(S), Map, _Val)
                then    write(S, !SubIO), nl(!SubIO),
                        print_sit(Map, S, !SubIO),
                        print_sit_info(Map, S, !SubIO),
                        (   if      R = finished
                            then    %draw_traces_incl_subsits(Map, S, !SubIO)
                                    %draw_trace(Map, S, !SubIO)
                                    true
                            else    true
                        ),
                        write_string("Remaining program: ", !SubIO),
                        print_prog(Map, P, !SubIO), nl(!SubIO)
                else    write_string("solving failed\n", !SubIO)
            ),
            write_string("Remaining program: ", !SubIO),
            write(P, !SubIO), nl(!SubIO),
            nl(!SubIO),
            IO1 = !.SubIO
        )
    ), Results, !IO),
    (   if      Results \= []
        then    foldl((pred(s_state(_, R)::in, {N, M}::in, {N1, M1}::out) is det :-
                    if      R = finished
                    then    N1 = N + 1, M1 = M + 1
                    else    N1 = N,     M1 = M + 1
                ), Results, {0, 0}, {Finished, Total}),
                format("percentage = %d / %d = %.2f\n",
                       [i(Finished), i(Total),
                        f(float(Finished) / float(Total))], !IO)
        else    format("percentage = nan\n", [], !IO)
    ),
    format("usertime = %f\n", [f(usertime(Tms2, Tms3))], !IO),
    format("systime = %f\n", [f(systime(Tms2, Tms3))], !IO).

%-----------------------------------------------------------------------------%
:- end_module cars.
%-----------------------------------------------------------------------------%

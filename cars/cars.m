%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: cars.m.
% Main author: schwering.
%
% Main predicate that takes observations from stdin and performs plan
% recognition.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module cars.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bat.
:- import_module int.
:- import_module io_util.
:- import_module float.
:- import_module list.
:- import_module obs.
:- import_module planrecog.
:- import_module prgolog.ccfluent.
:- import_module prgolog.nice.
:- import_module string.
:- import_module times.
:- import_module types.
%:- import_module table_statistics.

%-----------------------------------------------------------------------------%

main(!IO) :-
    times(Tms2, !IO),
    Prog = p(cruise(a)) // p(overtake(b, a)),
    planrecog(10, input_init_obs, input_next_obs, Prog, Results, !IO),
    times(Tms3, !IO),
    map0_io((pred(s_state(conf(P, S), R)::in, IO0::di, IO1::uo) is det :-
        some [!SubIO] (
            IO0 = !:SubIO,
            write(R, !SubIO), nl(!SubIO),
            (   if      solve(vargen(S), constraints(S), Map, _Val)
                then    print_sit(Map, S, !SubIO),
                        print_sit_info(Map, S, !SubIO),
                        (   if      R = finished
                            then    %draw_traces_incl_subsits(Map, S, !SubIO)
                                    draw_trace(Map, S, !SubIO)
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

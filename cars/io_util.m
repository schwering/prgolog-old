%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: cars.m.
% Main author: schwering.
%
% Basic action theory (BAT) for driving with two simple actions, set_yaw and
% set_veloc that control the steering and speed of the vehicle.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module io_util.

:- interface.

:- import_module assoc_list.
:- import_module bat.
:- import_module io.
:- import_module list.
:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module string.

%-----------------------------------------------------------------------------%

:- pred map0_io((pred(T1, io, io)::in(pred(in, di, uo) is det)),
               list(T1)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred print_sit(assoc_list(var, float)::in, sit(prim)::in,
                  io::di, io::uo) is det.

:- pred print_sit(output_stream::in,
                  string::in,
                  assoc_list(var, float)::in, sit(prim)::in,
                  io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred print_action(output_stream::in,
                      assoc_list(var, number)::in, prim::in,
                     io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred print_sit_with_info(assoc_list(var, number)::in, sit(prim)::in,
                            io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred print_sit_info(assoc_list(var, number)::in, sit(prim)::in,
                       io::di, io::uo) is det.

:- pred print_sit_info(output_stream::in,
                       assoc_list(var, number)::in, sit(prim)::in,
                       io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred draw_trace(assoc_list(var, number)::in, sit(prim)::in,
                   io::di, io::uo) is det.

:- pred draw_traces_incl_subsits(assoc_list(var, number)::in, sit(prim)::in,
                   io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.
:- import_module require.
:- import_module string.
:- import_module types.

%-----------------------------------------------------------------------------%

map0_io(_, [], !IO).
map0_io(P, [X | Xs], !IO) :- P(X, !IO), map0_io(P, Xs, !IO).

%-----------------------------------------------------------------------------%

print_action(Stream, Map, set_veloc(A, Mps, Tol, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "set_veloc(%s, %f, %f, %f)\n",
           [s(agent_to_string(A)), f(Mps), f(Tol), f(T)], !IO).
print_action(Stream, Map, set_yaw(A, L, Rad, Tol, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "set_yaw(%s, %s, %f, %f, %f)\n",
           [s(agent_to_string(A)), s(lane_to_string(L)),
            f(Rad), f(Tol), f(T)], !IO).
print_action(Stream, Map, wait_for(_, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "wait_for(..., %f)\n",
           [f(T)], !IO).
print_action(Stream, Map, match(OTime, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "match(%f, ..., %f)\n",
           [f(OTime), f(T)], !IO).
print_action(Stream, Map, eval(_, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "eval(..., %f)\n",
           [f(T)], !IO).
print_action(Stream, _, A @ init_env(_, _), !IO) :-
    write(Stream, A, !IO), nl(Stream, !IO).
print_action(Stream, _, seed(Seed), !IO) :-
    format(Stream, "seed(%d)\n",
           [i(Seed)], !IO).

%-----------------------------------------------------------------------------%

print_sit(Map, S, !IO) :- print_sit_2(stdout_stream, "", Map, S, 1, _, !IO).


print_sit(Stream, Prefix, Map, S, !IO) :-
    print_sit_2(Stream, Prefix, Map, S, 1, _, !IO).


:- pred print_sit_2(output_stream::in,
                    string::in,
                    assoc_list(var, float)::in, sit(prim)::in,
                    int::in, int::out, io::di, io::uo) is det.

print_sit_2(_, _, _, s0, !N, !IO).
print_sit_2(Stream, Prefix, Map, do(A, S), !.N, !:N, !IO) :-
    print_sit_2(Stream, Prefix, Map, S, !N, !IO),
    write_string(Stream, Prefix, !IO),
    write_string(Stream, " ", !IO),
    write(Stream, !.N, !IO),
    write_string(Stream, ": ", !IO),
    !:N = !.N + 1,
    print_action(Stream, Map, A, !IO).

%-----------------------------------------------------------------------------%

print_sit_with_info(Map, S, !IO) :-
    print_sit_with_info(stdout_stream, Map, S, !IO).


:- pred print_sit_with_info(output_stream::in,
                            assoc_list(var, number)::in, sit(prim)::in,
                            io::di, io::uo) is det.

print_sit_with_info(Stream, Map, s0, !IO) :-
    write_string(Stream, "initial situation", !IO), nl(!IO),
    print_sit_info(Stream, Map, s0, !IO),
    nl(Stream, !IO).
print_sit_with_info(Stream, Map, S1 @ do(A, S), !IO) :-
    print_sit_with_info(Stream, Map, S, !IO),
    print_action(Stream, Map, A, !IO),
    print_sit_info(Stream, Map, S1, !IO),
    nl(Stream, !IO).

%-----------------------------------------------------------------------------%

print_sit_info(Map, S, !IO) :- print_sit_info(stdout_stream, Map, S, !IO).

print_sit_info(Stream, Map, S, !IO) :-
    E = ( func(T) = eval_float(Map, T) ),
    format(Stream, "veloc(b, S) = %.1f\n", [f(veloc(b, S))], !IO),
    format(Stream, "yaw(b, S) = %.1f\n", [f(yaw(b, S))], !IO),
    format(Stream, "start(S) = %.1f\n", [f(E(start(S)))], !IO),
    format(Stream, "x(b, S) = %.1f\n", [f(E(x(b, S)(start(S))))], !IO),
    format(Stream, "y(b, S) = %.1f\n", [f(E(y(b, S)(start(S))))], !IO),
    format(Stream, "x_tol(b, S) = %.1f\n", [f(x_tol(b, S))], !IO),
    format(Stream, "y_tol(b, S) = %.1f\n", [f(y_tol(b, S))], !IO),
    format(Stream, "now(S) = %.1f\n", [f(E(now(S)(start(S))))], !IO),
    nl(Stream, !IO).

%-----------------------------------------------------------------------------%

:- pred open_next_file(string::in, output_stream::out, io::di, io::uo) is det.

open_next_file(Format, Stream, !IO) :-
    open_next_file_2(Format, 0, 999, Stream, !IO).


:- pred open_next_file_2(string::in, int::in, int::in, output_stream::out,
                         io::di, io::uo) is det.

open_next_file_2(Format, I, N, Stream, !IO) :-
    if      I > N
    then    error("cannot open file with pattern "++ Format)
    else    Filename = format(Format, [i(I)]),
            open_input(Filename, TestRes, !IO),
            (   if      TestRes = ok(TestStream)
                then    close_input(TestStream, !IO),
                        open_next_file_2(Format, I + 1, N, Stream, !IO)
                else    open_output(Filename, Res, !IO),
                        (   if      Res = ok(Stream0)
                            then    Stream = Stream0
                            else    error("cannot open non-existing file " ++
                                          "with pattern " ++ Format ++
                                          " for " ++ int_to_string(I))
                        )
            ).

%-----------------------------------------------------------------------------%

draw_trace(Map, S, !IO) :-
    open_next_file("traces/%04d.dat", Stream, !IO),
    print_sit(Stream, "# ", Map, S, !IO),
    draw_trace_2(Stream, Map, S, !IO),
    close_output(Stream, !IO).


:- pred draw_trace_2(output_stream::in,
                     assoc_list(var, number)::in, sit(prim)::in,
                     io::di, io::uo) is det.


draw_trace_2(Stream, Map, S, !IO) :-
    Agent = b,
    (   if   S = do(A, S0), A \= init_env(_, _)
        then draw_trace_2(Stream, Map, S0, !IO)
        else format(Stream, "time     xobs     yobs     xmod     ymod      xlo      ylo      xhi      yhi\n", [], !IO)
    ),
    (   (   if      S = do(match(_, Obs, _, _, _), _)
            then    (   if      Obs = {_, Agent, X0, Y0, _, _, _}
                        then    ObsX = format("%7.3f", [f(X0)]),
                                ObsY = format("%7.3f", [f(Y0)])
                        else if Obs = {_, _, _, _, Agent, X0, Y0}
                        then    ObsX = format("%7.3f", [f(X0)]),
                                ObsY = format("%7.3f", [f(Y0)])
                        else    error("invalid observation does not contain driver")
                    )
            else    ObsX = "    NaN", ObsY = "    NaN"
        ),
        E = ( func(T) = eval_float(Map, T) ),
        Time = E(start(S)),
        ModX = E(x(Agent, S)(start(S))),
        ModY = E(y(Agent, S)(start(S))),
        ModXTol = x_tol(Agent, S),
        ModYTol = y_tol(Agent, S),
        format(Stream, "%7.3f  %s  %s  %7.3f  %7.3f  %7.3f  %7.3f  %7.3f  %7.3f\n",
                       [f(Time), s(ObsX), s(ObsY), f(ModX), f(ModY),
                        f(ModX - ModXTol), f(ModY - ModYTol),
                        f(ModX + ModXTol), f(ModY + ModYTol)], !IO)
    ).

%-----------------------------------------------------------------------------%

draw_traces_incl_subsits(_, s0, !IO).
draw_traces_incl_subsits(Map, S1 @ do(_, S), !IO) :-
    draw_traces_incl_subsits(Map, S, !IO),
    draw_trace(Map, S1, !IO).

%-----------------------------------------------------------------------------%
:- end_module io_util.
%-----------------------------------------------------------------------------%

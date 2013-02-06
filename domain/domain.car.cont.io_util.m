%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.cont.io_util.m.
% Main author: schwering.
%
% Some I/O-related utilities such as printing situation terms.
%
%-----------------------------------------------------------------------------%

:- module domain.car.cont.io_util.

:- interface.

:- import_module assoc_list.
:- import_module io.
:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module string.

%-----------------------------------------------------------------------------%

:- pred print_sit(assoc_list(var, float)::in, sit::in,
                  io::di, io::uo) is det.

:- pred print_sit(output_stream::in,
                  string::in,
                  assoc_list(var, float)::in, sit::in,
                  io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred print_action(output_stream::in, assoc_list(var, number)::in, prim::in,
                     io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred print_prog(assoc_list(var, number)::in, prog::in,
                   io::di, io::uo) is det.

:- pred print_prog(output_stream::in, assoc_list(var, number)::in,
                   prog::in,
                   io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred print_sit_with_info(assoc_list(var, number)::in, sit::in,
                            io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred print_sit_info(assoc_list(var, number)::in, sit::in, agent::in,
                       io::di, io::uo) is det.

:- pred print_sit_info(output_stream::in,
                       assoc_list(var, number)::in, sit::in, agent::in,
                       io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred draw_trace(assoc_list(var, number)::in, sit::in,
                   io::di, io::uo) is det.

:- pred draw_traces_incl_subsits(assoc_list(var, number)::in, sit::in,
                                 io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

print_action(Stream, Map, set_veloc(A, Mps, Tol, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "set_veloc(%s, %f, %f, %f)",
           [s(agent_to_string(A)), f(Mps), f(Tol), f(T)], !IO).
print_action(Stream, Map, set_yaw(A, L, Rad, Tol, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "set_yaw(%s, %s, %f, %f, %f)",
           [s(agent_to_string(A)), s(lane_to_string(L)),
            f(Rad), f(Tol), f(T)], !IO).
print_action(Stream, Map, wait_for(_, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "wait_for(..., %f)",
           [f(T)], !IO).
print_action(Stream, Map, match(_, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "match(..., %f)",
           [f(T)], !IO).
print_action(Stream, Map, eval(_, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "eval(..., %f)",
           [f(T)], !IO).
print_action(Stream, _, A @ init_env(_), !IO) :-
    write(Stream, A, !IO).
print_action(Stream, _, seed(Seed), !IO) :-
    format(Stream, "seed(%d)",
           [i(Seed)], !IO).

%-----------------------------------------------------------------------------%

print_prog(Map, P, !IO) :- print_prog(stdout_stream, Map, P, !IO).


print_prog(Stream, Map, P, !IO) :- print_prog_2(Stream, Map, "", P, !IO).


:- pred print_prog_2(output_stream::in, assoc_list(var, number)::in,
                     string::in, prog::in,
                     io::di, io::uo) is det.

print_prog_2(Stream, Map, Op, seq(P1, P2), !IO) :-
    ThisOp = ";",
    ( if Op = "" ; Op \= ThisOp then write_string("(", !IO) else true ),
    print_prog_2(Stream, Map, ThisOp, P1, !IO),
    write_string(Stream, " ; ", !IO),
    print_prog_2(Stream, Map, ThisOp, P2, !IO),
    ( if Op = "" ; Op \= ThisOp then write_string(")", !IO) else true ).
print_prog_2(Stream, Map, Op, non_det(P1, P2), !IO) :-
    ThisOp = "|",
    ( if Op = "" ; Op \= ThisOp then write_string("(", !IO) else true ),
    print_prog_2(Stream, Map, ThisOp, P1, !IO),
    write_string(Stream, " | ", !IO),
    print_prog_2(Stream, Map, ThisOp, P2, !IO),
    ( if Op = "" ; Op \= ThisOp then write_string(")", !IO) else true ).
print_prog_2(Stream, Map, Op, conc(P1, P2), !IO) :-
    ThisOp = "||",
    ( if Op = "" ; Op \= ThisOp then write_string("(", !IO) else true ),
    print_prog_2(Stream, Map, ThisOp, P1, !IO),
    write_string(Stream, " || ", !IO),
    print_prog_2(Stream, Map, ThisOp, P2, !IO),
    ( if Op = "" ; Op \= ThisOp then write_string(")", !IO) else true ).
print_prog_2(Stream, Map, _, pick(_, X0, P), !IO) :-
    ThisOp = "pick",
    write_string(Stream, "pick(", !IO),
    write(X0, !IO),
    write_string(Stream, " ..., ", !IO),
    print_prog_2(Stream, Map, ThisOp, P(X0), !IO),
    write_string(Stream, ")", !IO).
print_prog_2(Stream, Map, _, star(P), !IO) :-
    ThisOp = "*",
    write_string(Stream, "(", !IO),
    print_prog_2(Stream, Map, ThisOp, P, !IO),
    write_string(Stream, ")*", !IO).
print_prog_2(Stream, _, _, proc(P), !IO) :-
    write(Stream, P, !IO).
print_prog_2(Stream, Map, _, pseudo_atom(complex(P)), !IO) :-
    ThisOp = "atomic",
    write_string(Stream, "atomic(", !IO),
    print_prog_2(Stream, Map, ThisOp, P, !IO),
    write_string(Stream, ")", !IO).
print_prog_2(Stream, Map, _, pseudo_atom(atom(P)), !IO) :-
    (   P = prim(A), print_action(Stream, Map, A, !IO)
    ;   P = primf(B), write(Stream, B, !IO)
    ;   P = test(_), write_string(Stream, "(...)?", !IO)
    ).
print_prog_2(Stream, _, _, nil, !IO) :-
    write_string(Stream, "nil", !IO).

%-----------------------------------------------------------------------------%

print_sit(Map, S, !IO) :- print_sit_2(stdout_stream, "", Map, S, 1, _, !IO).


print_sit(Stream, Prefix, Map, S, !IO) :-
    print_sit_2(Stream, Prefix, Map, S, 1, _, !IO).


:- pred print_sit_2(output_stream::in,
                    string::in,
                    assoc_list(var, float)::in, sit::in,
                    int::in, int::out, io::di, io::uo) is det.

print_sit_2(_, _, _, s0, !N, !IO).
print_sit_2(Stream, Prefix, Map, do(A, S), !.N, !:N, !IO) :-
    print_sit_2(Stream, Prefix, Map, S, !N, !IO),
    write_string(Stream, Prefix, !IO),
    write_string(Stream, " ", !IO),
    write(Stream, !.N, !IO),
    write_string(Stream, ": ", !IO),
    !:N = !.N + 1,
    print_action(Stream, Map, A, !IO), nl(Stream, !IO).

%-----------------------------------------------------------------------------%

print_sit_with_info(Map, S, !IO) :-
    print_sit_with_info(stdout_stream, Map, S, !IO).


:- pred print_sit_with_info(output_stream::in,
                            assoc_list(var, number)::in, sit::in,
                            io::di, io::uo) is det.

print_sit_with_info(Stream, Map, s0, !IO) :-
    write_string(Stream, "initial situation", !IO), nl(!IO),
    print_sit_info(Stream, Map, s0, c, !IO),
    nl(Stream, !IO).
print_sit_with_info(Stream, Map, S1 @ do(A, S), !IO) :-
    print_sit_with_info(Stream, Map, S, !IO),
    print_action(Stream, Map, A, !IO), nl(Stream, !IO),
    print_sit_info(Stream, Map, S1, c, !IO),
    nl(Stream, !IO).

%-----------------------------------------------------------------------------%

print_sit_info(Map, S, Agent, !IO) :-
    print_sit_info(stdout_stream, Map, S, Agent, !IO).

print_sit_info(Stream, Map, S, Agent, !IO) :-
    E = ( func(T) = eval_float(Map, T) ),
    N = agent_to_string(Agent),
    format(Stream, "veloc(%s, S) = %.1f\n", [s(N), f(veloc(Agent, S))], !IO),
    format(Stream, "yaw(%s, S) = %.1f\n", [s(N), f(yaw(Agent, S))], !IO),
    format(Stream, "start(S) = %.1f\n", [f(E(start(S)))], !IO),
    format(Stream, "x(%s, S) = %.1f\n", [s(N), f(E(x(Agent, S)(start(S))))], !IO),
    format(Stream, "y(%s, S) = %.1f\n", [s(N), f(E(y(Agent, S)(start(S))))], !IO),
    format(Stream, "x_tol(%s, S) = %.1f\n", [s(N), f(x_tol(Agent, S))], !IO),
    format(Stream, "y_tol(%s, S) = %.1f\n", [s(N), f(y_tol(Agent, S))], !IO),
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
                     assoc_list(var, number)::in, sit::in,
                     io::di, io::uo) is det.


draw_trace_2(Stream, Map, S, !IO) :-
    Agent = b `with_type` agent,
    (   if   S = do(A, S0), A \= init_env(_)
        then draw_trace_2(Stream, Map, S0, !IO)
        else format(Stream, "time     xobs     yobs     xmod     ymod      xlo      ylo      xhi      yhi\n", [], !IO)
    ),
    (   (   if      S = do(match(Obs, _, _, _), _)
            then    (   if      some [X, Y] ( X = x_pos(Obs, Agent), Y = y_pos(Obs, Agent) )
                        then    ObsX = format("%7.3f", [f(X)]),
                                ObsY = format("%7.3f", [f(Y)])
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
:- end_module domain.car.cont.io_util.
%-----------------------------------------------------------------------------%

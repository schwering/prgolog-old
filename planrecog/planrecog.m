%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: planrecog.m.
% Main author: schwering.
%
% Plan recognition-specific utilities.
%
% planrecog/6 spawns a given number of sampling processes and thus
% approximates the probability that the given program explains the observations
% emitted from the observation source. A pretty detailed result is given in the
% form of list of s_state objects, one for each sample.
%
% online_planrecog/7 works similarly except that it calls a handler after each
% step.
%
%-----------------------------------------------------------------------------%

:- module planrecog.

:- interface.

:- import_module io.
:- import_module list.
:- import_module domain.
:- import_module prgolog.
:- import_module prgolog.nice.
:- import_module thread.mvar.

%-----------------------------------------------------------------------------%

:- type s_phase ---> running ; finishing ; finished ; failed.
:- type s_state(A) ---> s_state(conf(A), s_phase).

:- type handler(A) == (pred(int, s_state(A), io, io)).
:- inst handler == (pred(in, in, di, uo) is det).

%-----------------------------------------------------------------------------%

:- pred planrecog(int::in,
                  Source::in,
                  prog(A)::in,
                  list(s_state(A))::out,
                  io::di, io::uo) is cc_multi
    <= (pr_bat(A, Obs, Env), obs_source(Obs, Env, Source, _)).

%-----------------------------------------------------------------------------%

:- pred empty_handler `with_type` handler(_).
:- mode empty_handler `with_inst` handler.

:- pred online_planrecog(int::in,
                         Source::in,
                         list(mvar(s_state(A)))::out,
                         handler(A)::in(handler),
                         prog(A)::in,
                         io::di, io::uo) is cc_multi
    <= (pr_bat(A, Obs, Env), obs_source(Obs, Env, Source, _)).

:- pred wait_for_planrecog_finish(Source::in, list(mvar(s_state(A)))::in,
                                  io::di, io::uo) is det
    <= obs_source(_, _, Source, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module domain.car.
:- import_module domain.car.obs.
:- import_module prgolog.
:- import_module prgolog.nice.
:- import_module thread.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type logger(A) == (pred(s_state(A), io, io)).
:- inst logger == (pred(in, di, uo) is det).


:- pred empty_logger `with_type` logger(_).
:- mode empty_logger `with_inst` logger.

empty_logger(_, !IO).


empty_handler(_, _, !IO).

%-----------------------------------------------------------------------------%

:- pred thread_id(int::out, io::di, io::uo) is det.

:- pragma foreign_proc(c, thread_id(Id::out, IO0::di, IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    Id = (int) pthread_self();
    IO = IO0;
").


:- pred merge_and_trans_loop(Source::in, logger(A)::in(logger),
                             s_state(A)::in, s_state(A)::out,
                             ObsStreamState::di, ObsStreamState::uo,
                             io::di, io::uo) is det
    <= (pr_bat(A, Obs, Env), obs_source(Obs, Env, Source, ObsStreamState)).

merge_and_trans_loop(Source, Log, !State, !ObsStreamState, !IO) :-
    merge_and_trans(!State, !ObsStreamState, Cont, !IO),
    Log(!.State, !IO),
    (   if      Cont = yes
        then    merge_and_trans_loop(Source, Log, !State, !ObsStreamState, !IO)
        else    true
    ).


:- pred merge_and_trans(s_state(A)::in,
                        s_state(A)::out,
                        ObsStreamState::di, ObsStreamState::uo,
                        bool::out,
                        io::di, io::uo) is det
    <= (pr_bat(A, Obs, Env), obs_source(Obs, Env, _, ObsStreamState)).

merge_and_trans(s_state(conf(P, S), !.Phase), s_state(conf(P2, S2), !:Phase),
                !ObsStreamState, Continue, !IO) :-
    if
        !.Phase \= finishing,
        obs_count_in_prog(P) < lookahead(S)
    then
        Continue = yes,
        next_obs(ObsMsg, S, P, !ObsStreamState, !IO),
        P2 = ( if ObsMsg = obs_msg(Obs) then append_obs(P, Obs) else P ),
        S2 = ( if ObsMsg = init_msg(E) then init_env_sit(E, S) else S ),
        !:Phase = ( if ObsMsg = end_of_obs then finishing else running )
    else if
        final(remove_obs_sequence(P), S),
        last_action_covered_by_obs(S)
    then
        Continue = no,
        P2 = P,
        S2 = S,
        !:Phase = finished
    else if
        trans(P, S, P1, S1)
    then
        Continue = yes,
        P2 = P1,
        S2 = S1,
        !:Phase = !.Phase
    else
        Continue = no,
        P2 = P,
        S2 = S,
        !:Phase = failed.

%-----------------------------------------------------------------------------%

:- pred run_concurrently_par_conj(int::in,
                                  pred(int, s_state(A))
                                    ::in(pred(in, out) is det),
                                  list(s_state(A))::out) is det.

run_concurrently_par_conj(I, P, Rs) :-
    if      I > 0
    then    ( P(I, R) & run_concurrently_par_conj(I - 1, P, Rs0) ),
            Rs = [R | Rs0]
    else    Rs = [].


:- pred run_sequentially(int::in,
                         pred(int, s_state(A))::in(pred(in, out) is det),
                         list(s_state(A))::out) is det.

run_sequentially(I, P, Rs) :-
    if      I > 0
    then    P(I, R), run_sequentially(I - 1, P, Rs0),
            Rs = [R | Rs0]
    else    Rs = [].


:- pred init_vars(int::in, list(mvar(T))::out, io::di, io::uo) is det.

init_vars(N, Vs, !IO) :-
    if      N > 0
    then    Vs = [V | Vs0],
            init(V, !IO),
            init_vars(N - 1, Vs0, !IO)
    else    Vs = [].


:- pred take_vars(list(mvar(T))::in, list(T)::out, io::di, io::uo) is det.

take_vars([], [], !IO).
take_vars([V | Vs], [R | Rs], !IO) :-
    take(V, R, !IO),
    take_vars(Vs, Rs, !IO).


:- pred run_concurrently_thread(Source::in, int::in,
                                list(mvar(s_state(A)))::in,
                                pred(int, s_state(A), io, io)
                                    ::in(pred(in, out, di, uo) is det),
                                io::di, io::uo) is cc_multi
                                <= obs_source(_, _, Source, _).

run_concurrently_thread(_, _, [], _, !IO).
run_concurrently_thread(Source, N, [V | Vs], P, !IO) :-
    spawn((pred(!.SubIO::di, !:SubIO::uo) is cc_multi :-
        P(N, R, !SubIO),
        R = s_state(_, Phase),
        (   Phase = running,   update_state(Source, N, working,  !SubIO)
        ;   Phase = finishing, update_state(Source, N, working,  !SubIO)
        ;   Phase = finished,  update_state(Source, N, finished, !SubIO)
        ;   Phase = failed,    update_state(Source, N, failed,   !SubIO)
        ),
        put(V, R, !SubIO)
    ), !IO),
    run_concurrently_thread(Source, N - 1, Vs, P, !IO).


:- pred run_concurrently(Source::in, int::in,
                         pred(int, s_state(A), io, io)
                            ::in(pred(in, out, di, uo) is det),
                         list(s_state(A))::out,
                         io::di, io::uo) is cc_multi
                         <= obs_source(_, _, Source, _).

%run_concurrently(N, P, Rs, !IO) :- run_concurrently_par_conj(N, P, Rs).
%run_concurrently(N, P, Rs, !IO) :- run_sequentially(N, P, Rs).
run_concurrently(Source, N, P, Rs, !IO) :-
    init_vars(N, Vs, !IO),
    run_concurrently_thread(Source, N, Vs, P, !IO),
    take_vars(Vs, Rs, !IO).

%-----------------------------------------------------------------------------%

:- pred pr_thread(Source::in, prog(A)::in, int::in, s_state(A)::out,
                  io::di, io::uo) is det
    <= (pr_bat(A, Obs, Env), obs_source(Obs, Env, Source, StreamState)).

pr_thread(Source, Prog, I, R, !IO) :-
    Log = empty_logger,
    InitialState = s_state(conf(Prog, seed_init_sit(I)), running),
    init_obs_stream(Source, I, ObsStreamState, !IO),
    merge_and_trans_loop(Source, Log, InitialState, R, ObsStreamState, _, !IO).


planrecog(ThreadCount, Source, Prog, Results, !IO) :-
    Thread = pr_thread(Source, Prog),
    run_concurrently(Source, ThreadCount, Thread, Results, !IO).

%-----------------------------------------------------------------------------%

:- pred opr_thread(Source::in, handler(A)::in(handler),
                   prog(A)::in, int::in, s_state(A)::out,
                   io::di, io::uo) is det
    <= (pr_bat(A, Obs, Env), obs_source(Obs, Env, Source, StreamState)).

opr_thread(Source, Handler, Prog, I, R, !IO) :-
    Log = (pred(S::in, !.IO::di, !:IO::uo) is det :-
        Handler(I, S, !IO)
    ),
    InitialState = s_state(conf(Prog, seed_init_sit(I)), running),
    init_obs_stream(Source, I, ObsStreamState, !IO),
    merge_and_trans_loop(Source, Log, InitialState, R, ObsStreamState, _, !IO).


online_planrecog(ThreadCount, Source, Vars, Handler, Prog, !IO) :-
    init_vars(ThreadCount, Vars, !IO),
    Thread = opr_thread(Source, Handler, Prog),
    run_concurrently_thread(Source, ThreadCount, Vars, Thread, !IO).


wait_for_planrecog_finish(Source, Vars, !IO) :-
    mark_obs_end(Source, !IO),
    take_vars(Vars, _, !IO).

%-----------------------------------------------------------------------------%
:- end_module planrecog.
%-----------------------------------------------------------------------------%

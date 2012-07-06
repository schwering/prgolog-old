%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: planrecog.m.
% Main author: schwering.
%
% Plan recognition-specific utilities.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module planrecog.

:- interface.

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module obs.
:- import_module prgolog.
:- import_module prgolog.nice.
:- import_module thread.mvar.
:- import_module types.

%-----------------------------------------------------------------------------%

:- type s_phase ---> running ; finishing ; finished ; failed.
:- type s_state(A, B, P) ---> s_state(conf(A, B, P), s_phase).

:- type handler(A, B, P) == (pred(int, s_state(A, B, P), io, io)).
:- inst handler == (pred(in, in, di, uo) is det).

%-----------------------------------------------------------------------------%

:- pred planrecog(int::in,
                  init_obs(T)::in(init_obs),
                  next_obs(A, B, P, T)::in(next_obs),
                  prog(A, B, P)::in,
                  list(s_state(A, B, P))::out,
                  io::di, io::uo) is cc_multi <= pr_bat(A, B, P).

%-----------------------------------------------------------------------------%

:- pred empty_handler(int::in, s_state(A, B, P)::in, io::di, io::uo) is det.

:- pred online_planrecog(int::in, list(mvar(s_state(A, B, P)))::out,
                         handler(A, B, P)::in(handler),
                         prog(A, B, P)::in,
                         io::di, io::uo) is cc_multi <= pr_bat(A, B, P).

:- pred wait_for_planrecog_finish(list(mvar(s_state(A, B, P)))::in,
                                  io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module prgolog.
:- import_module prgolog.nice.
:- import_module thread.
:- import_module types.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type logger(A, B, P) == (pred(s_state(A, B, P), io, io)).
:- inst logger == (pred(in, di, uo) is det).


:- pred empty_logger(s_state(A, B, P)::in, io::di, io::uo) is det.

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


:- pred merge_and_trans_loop(logger(A, B, P)::in(logger),
                             next_obs(A, B, P, T)::in(next_obs),
                             s_state(A, B, P)::in,
                             s_state(A, B, P)::out,
                             T::di, T::uo) is det <= pr_bat(A, B, P).

merge_and_trans_loop(Log, NextObs, !State, !ObsGenState) :-
    merge_and_trans(NextObs, !State, !ObsGenState, Cont),
    trace [io(!IO)] (
        Log(!.State, !IO)
    ),
    (   if      Cont = yes
        then    merge_and_trans_loop(Log, NextObs, !State, !ObsGenState)
        else    true
    ).


:- pred merge_and_trans(next_obs(A, B, P, T)::in(next_obs),
                        s_state(A, B, P)::in,
                        s_state(A, B, P)::out,
                        T::di, T::uo,
                        bool::out) is det <= pr_bat(A, B, P).

merge_and_trans(NextObs,
                s_state(conf(P, S), !.Phase),
                s_state(conf(P2, S2), !:Phase),
                !ObsGenState,
                Continue) :-
    (   if
            !.Phase \= finishing,
            match_count_in_prog(P) < lookahead(S)
        then
            Continue = yes,
            NextObs(ObsMsg, S, P, !ObsGenState),
            P2 = ( if ObsMsg = obs_msg(Obs) then append_obs(P, Obs) else P ),
            S2 = ( if ObsMsg = init_msg(Map, T) then init_env_sit(T, Map, S) else S ),
            !:Phase = ( if ObsMsg = end_of_obs then finishing else running )
        else if
            final(remove_match_sequence(P), S),
            last_action_covered_by_match(S)
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
            !:Phase = failed
    ).

%-----------------------------------------------------------------------------%

:- pred run_concurrently_par_conj(int::in,
                                  pred(int, s_state(A, B, P))::in(pred(in, out) is det),
                                  list(s_state(A, B, P))::out) is det.

run_concurrently_par_conj(I, P, Rs) :-
    (   if      I > 0
        then    ( P(I, R) & run_concurrently_par_conj(I - 1, P, Rs0) ),
                Rs = [R | Rs0]
        else    Rs = []
    ).


:- pred run_sequentially(int::in,
                         pred(int, s_state(A, B, P))::in(pred(in, out) is det),
                         list(s_state(A, B, P))::out) is det.

run_sequentially(I, P, Rs) :-
    (   if      I > 0
        then    P(I, R), run_sequentially(I - 1, P, Rs0),
                Rs = [R | Rs0]
        else    Rs = []
    ).


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


:- pred run_concurrently_thread(int::in,
                                list(mvar(s_state(A, B, P)))::in,
                                pred(int, s_state(A, B, P))::in(pred(in, out) is det),
                                io::di, io::uo) is cc_multi.

run_concurrently_thread(_, [], _, !IO).
run_concurrently_thread(I, [V | Vs], P, !IO) :-
    spawn((pred(IO0::di, IO1::uo) is cc_multi :-
        some [!SubIO] (
            !:SubIO = IO0,
            P(I, R),
            R = s_state(_, Phase),
            %write(I, !SubIO),
            %write_string(" --> ", !SubIO),
            %write(Phase, !SubIO),
            %nl(!SubIO),
            (   Phase = running, update_state(I, working, !SubIO)
            ;   Phase = finishing, update_state(I, working, !SubIO)
            ;   Phase = finished, update_state(I, finished, !SubIO)
            ;   Phase = failed, update_state(I, failed, !SubIO)
            ),
            put(V, R, !SubIO),
            !.SubIO = IO1
        )
    ), !IO),
    run_concurrently_thread(I - 1, Vs, P, !IO).


:- pred run_concurrently(int::in,
                         pred(int, s_state(A, B, P))::in(pred(in, out) is det),
                         list(s_state(A, B, P))::out,
                         io::di, io::uo) is cc_multi.

%run_concurrently(N, P, Rs, !IO) :- run_concurrently_par_conj(N, P, Rs).
%run_concurrently(N, P, Rs, !IO) :- run_sequentially(N, P, Rs).
run_concurrently(N, P, Rs, !IO) :-
    init_vars(N, Vs, !IO),
    run_concurrently_thread(N, Vs, P, !IO),
    take_vars(Vs, Rs, !IO).

%-----------------------------------------------------------------------------%

planrecog(ThreadCount, InitObs, NextObs, Prog, Results, !IO) :-
    Thread = (pred(I::in, R::out) is det :-
        InitialState = s_state(conf(Prog, seed_init_sit(I)), running),
        InitObs(I, InitialObsGenState),
        merge_and_trans_loop(empty_logger, NextObs, InitialState, R,
                             InitialObsGenState, _)
    ),
    run_concurrently(ThreadCount, Thread, Results, !IO).

%-----------------------------------------------------------------------------%

online_planrecog(ThreadCount, Vars, Handler, Prog, !IO) :-
    InitObs = global_init_obs,
    NextObs = global_next_obs,
    Thread = (pred(I::in, R::out) is det :-
        Log = (pred(S::in, IO0::di, IO1::uo) is det :-
            Handler(I, S, IO0, IO1)
        ),
        InitialState = s_state(conf(Prog, seed_init_sit(I)), running),
        InitObs(I, InitialObsGenState),
        merge_and_trans_loop(Log, NextObs, InitialState, R,
                             InitialObsGenState, _)
    ),
    init_vars(ThreadCount, Vars, !IO),
    run_concurrently_thread(ThreadCount, Vars, Thread, !IO).


wait_for_planrecog_finish(Vars, !IO) :-
    mark_observation_end(!IO),
    take_vars(Vars, _, !IO).

%-----------------------------------------------------------------------------%
:- end_module planrecog.
%-----------------------------------------------------------------------------%

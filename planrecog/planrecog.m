%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012-2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
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
:- use_module thread.mvar.

%-----------------------------------------------------------------------------%

:- type s_phase ---> running ; finishing ; finished ; failed.
:- type s_state(A) ---> s_state(conf(A), s_phase).

:- type handler(A) == (pred(int, s_state(A), s_state(A), io, io)).
:- inst handler == (pred(in, in, out, di, uo) is det).

%-----------------------------------------------------------------------------%

:- pred planrecog(int::in,
                  Source::in,
                  prog(A)::in,
                  list(s_state(A))::out,
                  io::di, io::uo) is cc_multi
    <= (pr_bat(A, Obs), obs_source(Obs, Source, _)).

%-----------------------------------------------------------------------------%

:- pred empty_handler `with_type` handler(_).
:- mode empty_handler `with_inst` handler.

:- pred online_planrecog(int::in,
                         Source::in,
                         list(mvar.mvar(s_state(A)))::out,
                         handler(A)::in(handler),
                         prog(A)::in,
                         io::di, io::uo) is cc_multi
    <= (pr_bat(A, Obs), obs_source(Obs, Source, _)).

:- pred wait_for_planrecog_finish(Source::in, list(mvar.mvar(s_state(A)))::in,
                                  io::di, io::uo) is det
    <= obs_source(_, Source, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module domain.car.
:- import_module domain.car.obs.
:- import_module list.
:- import_module prgolog.
:- import_module prgolog.nice.
:- import_module require.
:- import_module thread.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type logger(A) == (pred(s_state(A), s_state(A), io, io)).
:- inst logger == (pred(in, out, di, uo) is det).


:- pred empty_logger `with_type` logger(_).
:- mode empty_logger `with_inst` logger.

empty_logger(!State, !IO).


empty_handler(_, !State, !IO).

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
    <= (pr_bat(A, Obs), obs_source(Obs, Source, ObsStreamState)).

merge_and_trans_loop(Source, Log, !State, !ObsStreamState, !IO) :-
    merge_and_trans(!State, !ObsStreamState, Cont, !IO),
    Log(!State, !IO),
    (   if      Cont = yes
        then    merge_and_trans_loop(Source, Log, !State, !ObsStreamState, !IO)
        else    true
    ).


:- pred merge_and_trans(s_state(A)::in,
                        s_state(A)::out,
                        ObsStreamState::di, ObsStreamState::uo,
                        bool::out,
                        io::di, io::uo) is det
    <= (pr_bat(A, Obs), obs_source(Obs, _, ObsStreamState)).

merge_and_trans(s_state(conf(P, S), !.Phase), s_state(conf(P2, S2), !:Phase),
                !ObsStreamState, Continue, !IO) :-
    if
        !.Phase \= finishing,
        % XXX old behavior fixes bug that observations are not correctly
        % appended
        %(obs_count_in_prog(P) + 1) * obs_prog_length(P) =< lookahead(S)
        obs_trans_count(P) < lookahead(S)
    then
        Continue = yes,
        next_obs(ObsMsg, S, P, !ObsStreamState, !IO),
        P2 = ( if ObsMsg = obs_msg(Obs) then append_obs(P, Obs) else P ),
        S2 = ( if ObsMsg = init_msg(E) then init_env_sit(E, S) else S ),
        !:Phase = ( if ObsMsg = end_of_obs then finishing else running )
    else if
        % XXX tricky
        % final(substf_obs(func(Obs) = non_det(nil, Obs), P), S),
        final(subst_obs(nil, P), S),
        last_action_covered_by_obs(S)
    then
        Continue = no,
        P2 = P,
        S2 = S,
        !:Phase = finished
    else if
        % XXX tricky
        % not final(substf_obs(func(Obs) = non_det(nil, Obs), P), S),
        not final(subst_obs(nil, P), S), % XXX avoid nontermination due to star
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

    % The problem with built-in parallel conjunction is that it's
    % for pure/non-IO code, but P needs IO parameters.
:- pred run_par_conj(int::in,
                     pred(int, s_state(A))::in(pred(in, out) is det),
                     list(s_state(A))::out) is det.

run_par_conj(I, P, Rs) :-
    if      I > 0
    then    ( P(I, R) & run_par_conj(I - 1, P, Rs0) ),
            Rs = [R | Rs0]
    else    Rs = [].


:- pred run_seq(Source::in, int::in,
                list(mvar.mvar(s_state(A)))::out,
                pred(int, s_state(A), io, io)::in(pred(in, out, di, uo) is det),
                io::di, io::uo) is cc_multi <= obs_source(_, Source, _).

run_seq(Source, N, Vars, P, !IO) :-
    if N = 0 then
        Vars = []
    else if N > 0 then
        mvar.init(V, !IO),
        P(N, R, !IO),
        R = s_state(_, Phase),
        (   Phase = running,   update_state(Source, N, working,  !IO)
        ;   Phase = finishing, update_state(Source, N, working,  !IO)
        ;   Phase = finished,  update_state(Source, N, finished, !IO)
        ;   Phase = failed,    update_state(Source, N, failed,   !IO)
        ),
        mvar.put(V, R, !IO),
        run_seq(Source, N - 1, Vs, P, !IO),
        Vars = [V|Vs]
    else
        unexpected($module, $pred, "negative number of executions").


:- pred run_threads(Source::in, int::in,
                    list(mvar.mvar(s_state(A)))::out,
                    pred(int, s_state(A), io, io)::in(pred(in, out, di, uo) is det),
                    io::di, io::uo) is cc_multi <= obs_source(_, Source, _).

run_threads(Source, N, Vars, P, !IO) :-
    if N = 0 then
        Vars = []
    else if N > 0 then
        mvar.init(V, !IO),
        spawn((pred(!.SubIO::di, !:SubIO::uo) is cc_multi :-
            P(N, R, !SubIO),
            R = s_state(_, Phase),
            (   Phase = running,   update_state(Source, N, working,  !SubIO)
            ;   Phase = finishing, update_state(Source, N, working,  !SubIO)
            ;   Phase = finished,  update_state(Source, N, finished, !SubIO)
            ;   Phase = failed,    update_state(Source, N, failed,   !SubIO)
            ),
            mvar.put(V, R, !SubIO)
        ), !IO),
        run_threads(Source, N - 1, Vs, P, !IO),
        Vars = [V|Vs]
    else
        unexpected($module, $pred, "negative number of threads to start").

%-----------------------------------------------------------------------------%

:- pred pr_thread(Source::in, prog(A)::in, int::in, s_state(A)::out,
                  io::di, io::uo) is det
    <= (pr_bat(A, Obs), obs_source(Obs, Source, StreamState)).

pr_thread(Source, Prog, I, R, !IO) :-
    Log = empty_logger,
    InitialState = s_state(conf(Prog, seed_init_sit(I)), running),
    init_obs_stream(Source, I, ObsStreamState, !IO),
    merge_and_trans_loop(Source, Log, InitialState, R, ObsStreamState, _, !IO).


planrecog(ThreadCount, Source, Prog, Results, !IO) :-
    Thread = pr_thread(Source, Prog),
    run_threads(Source, ThreadCount, Vars, Thread, !IO),
    map_foldl(mvar.take, Vars, Results, !IO).

%-----------------------------------------------------------------------------%

:- pred opr_thread(Source::in, handler(A)::in(handler),
                   prog(A)::in, int::in, s_state(A)::out,
                   io::di, io::uo) is det
    <= (pr_bat(A, Obs), obs_source(Obs, Source, StreamState)).

opr_thread(Source, Handler, Prog, I, R, !IO) :-
    Log = (pred(!.State::in, !:State::out, !.IO::di, !:IO::uo) is det :-
        Handler(I, !State, !IO)
    ),
    InitialState = s_state(conf(Prog, seed_init_sit(I)), running),
    init_obs_stream(Source, I, ObsStreamState, !IO),
    merge_and_trans_loop(Source, Log, InitialState, R, ObsStreamState, _, !IO).


online_planrecog(ThreadCount, Source, Vars, Handler, Prog, !IO) :-
    Thread = opr_thread(Source, Handler, Prog),
    run_threads(Source, ThreadCount, Vars, Thread, !IO).


wait_for_planrecog_finish(Source, Vars, !IO) :-
    mark_obs_end(Source, !IO),
    map_foldl(mvar.take, Vars, _, !IO).

%-----------------------------------------------------------------------------%
:- end_module planrecog.
%-----------------------------------------------------------------------------%

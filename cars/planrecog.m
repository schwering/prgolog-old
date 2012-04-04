%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: planrecog.m.
% Main author: schwering.
%
% Basic action theory (BAT) for driving with two simple actions, set_yaw and
% set_veloc that control the steering and speed of the vehicle.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module planrecog.

:- interface.

:- import_module bat.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module obs.
:- import_module prgolog.
:- import_module prgolog.nice.

%-----------------------------------------------------------------------------%

:- type s_phase ---> running ; finishing ; finished ; failed.
:- type s_state ---> s_state(conf(prim, stoch, procedure), s_phase).

%-----------------------------------------------------------------------------%

:- pred merge_and_trans(next_obs(T)::in(next_obs),
                        s_state::in,
                        s_state::out,
                        T::di, T::uo,
                        bool::out) is det.

%-----------------------------------------------------------------------------%

:- pred planrecog(int::in,
                  init_obs(T)::in(init_obs),
                  next_obs(T)::in(next_obs),
                  prog(prim, stoch, procedure)::in,
                  list(s_state)::out,
                  io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module thread.
:- import_module thread.mvar.

%-----------------------------------------------------------------------------%

:- pred merge_and_trans_loop(next_obs(T)::in(next_obs),
                             s_state::in,
                             s_state::out,
                             T::di, T::uo) is det.

merge_and_trans_loop(NextObs, !State, !ObsGenState) :-
    merge_and_trans(NextObs, !State, !ObsGenState, Cont),
    (   if      Cont = yes
        then    merge_and_trans_loop(NextObs, !State, !ObsGenState)
        else    true
    ).


merge_and_trans(NextObs,
                s_state(conf(P, S), !.Phase),
                s_state(conf(P2, S2), !:Phase),
                !ObsGenState,
                Continue) :-
    (   if      !.Phase \= finishing,
                match_count(P) < bat.lookahead(S)
        then    NextObs(ObsMsg, !ObsGenState)
        else    ObsMsg = end_of_obs
    ),
    P0 = ( if ObsMsg = obs_msg(Obs) then append_obs(P, Obs) else P ),
    S0 = ( if ObsMsg = init_msg(Map, T) then do(init_env(T, Map), S) else S ),
    (   if
            !.Phase \= finishing,
            match_count(P) < bat.lookahead(S)
        then
            Continue = yes,
            P2 = P0,
            S2 = S0,
            !:Phase = ( if ObsMsg = end_of_obs then finishing else running )
        else if
            final(remove_match_sequence(P), S),
            last_action_covered_by_match(S)
        then
            Continue = no,
            P2 = P0,
            S2 = S0,
            !:Phase = finished
        else if
            trans(P0, S0, P1, S1)
        then
            Continue = yes,
            P2 = P1,
            S2 = S1,
            !:Phase = !.Phase
        else
            Continue = no,
            P2 = P0,
            S2 = S0,
            !:Phase = failed
    ).

%-----------------------------------------------------------------------------%

:- pred run_concurrently_par_conj(int::in,
                                  pred(int, s_state)::in(pred(in, out) is det),
                                  list(s_state)::out) is det.

run_concurrently_par_conj(I, P, Rs) :-
    (   if      I > 0
        then    ( P(I, R) & run_concurrently_par_conj(I - 1, P, Rs0) ),
                Rs = [R | Rs0]
        else    Rs = []
    ).


:- pred run_sequentially(int::in,
                         pred(int, s_state)::in(pred(in, out) is det),
                         list(s_state)::out) is det.

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
                                list(mvar(s_state))::in,
                                pred(int, s_state)::in(pred(in, out) is det),
                                io::di,
                                io::uo) is cc_multi.

run_concurrently_thread(_, [], _, !IO).
run_concurrently_thread(I, [V | Vs], P, !IO) :-
    spawn((pred(IO0::di, IO1::uo) is cc_multi :-
        P(I, R),
        put(V, R, IO0, IO1)
    ), !IO),
    run_concurrently_thread(I - 1, Vs, P, !IO).


:- pred run_concurrently(int::in,
                         pred(int, s_state)::in(pred(in, out) is det),
                         list(s_state)::out,
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
        InitialState = s_state(conf(Prog, do(seed(I), s0)), running),
        InitObs(InitialObsGenState),
        merge_and_trans_loop(NextObs, InitialState, R, InitialObsGenState, _)
    ),
    run_concurrently(ThreadCount, Thread, Results, !IO).

%-----------------------------------------------------------------------------%
:- end_module planrecog.
%-----------------------------------------------------------------------------%
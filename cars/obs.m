%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: obs.m.
% Main author: schwering.
%
% Types and operations for observations, particularly the generator that reads
% from stdin.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module obs.

:- interface.

:- import_module assoc_list.
:- import_module bat.
:- import_module types.

%-----------------------------------------------------------------------------%

:- type obs_msg ---> init_msg(assoc_list(agent, {mps, rad, m, m}), s)
                ;    obs_msg(obs)
                ;    end_of_obs.

:- type init_obs(T) == (pred(int, T)).
:- inst init_obs == (pred(in, uo) is det).

:- type next_obs(T) == (pred(obs_msg, sit, prog, T, T)).
:- inst next_obs == (pred(out, in, in, di, uo) is det).

%-----------------------------------------------------------------------------%

:- func append_match(prog, prim) = prog is det.

:- func remove_match_sequence(prog) = prog is semidet.

:- func last_match(sit) = prim is semidet.

:- pred last_action_covered_by_match(sit::in) is semidet.

:- func append_obs(prog, obs) = prog is det.

%-----------------------------------------------------------------------------%

:- pred input_init_obs(int::in, int::uo) is det.

:- pred input_next_obs(obs_msg::out, sit::in, prog::in,
                       int::di, int::uo) is det.

%-----------------------------------------------------------------------------%

:- pred global_init_obs(int::in, {int, int}::uo) is det.

:- pred global_next_obs(obs_msg::out, sit::in, prog::in,
                        {int, int}::di, {int, int}::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module solutions.

%-----------------------------------------------------------------------------%

append_match(P, O) =
    (   if      P1 = append_match_to_most_right(P, O)
        then    P1
        else    conc(P, pseudo_atom(atom(prim(O))))
    ).


:- func append_match_to_most_right(prog, prim) =
                                   prog is semidet.

append_match_to_most_right(seq(P1, P2), M) =
    (   if      Q2 = append_match_to_most_right(P2, M)
        then    seq(P1, Q2)
        else    append_match_to_most_right(P1, M) ).
append_match_to_most_right(non_det(P1, P2), M) =
    (   if      Q2 = append_match_to_most_right(P2, M)
        then    non_det(P1, Q2)
        else    append_match_to_most_right(P1, M) ).
append_match_to_most_right(conc(P1, P2), M) =
    (   if      Q2 = append_match_to_most_right(P2, M)
        then    conc(P1, Q2)
        else    append_match_to_most_right(P1, M) ).
append_match_to_most_right(star(P), M) =
    append_match_to_most_right(P, M).
append_match_to_most_right(M0, M) = seq(M0, pseudo_atom(atom(prim(M)))) :-
    M0 = pseudo_atom(atom(prim(match(_, _, _, _)))).


remove_match_sequence(conc(P1, P2)) = Q :-
    if          only_match_actions(P2)
    then        Q = P1
    else if     only_match_actions(P1)
    then        Q = P2
    else        false.


:- pred only_match_actions(prog::in) is semidet.

only_match_actions(seq(P1, P2)) :-
    only_match_actions(P1),
    only_match_actions(P2).
only_match_actions(non_det(P1, P2)) :-
    only_match_actions(P1),
    only_match_actions(P2).
only_match_actions(conc(P1, P2)) :-
    only_match_actions(P1),
    only_match_actions(P2).
only_match_actions(star(P)) :-
    only_match_actions(P).
only_match_actions(pseudo_atom(atom(prim(match(_, _, _, _))))).
only_match_actions(nil).


last_match(do(A, S)) =
    ( if A = match(_, _, _, _) then A else last_match(S) ).


last_action_covered_by_match(S) :-
    match(_, _, _, T0) = last_match(S),
    C = (start(S) `=` T0),
    solve(vargen(S), [C] ++ constraints(S)).


append_obs(P, O) = append_match(P, obs2match(O)).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "#include ""obs.h""").


:- pragma foreign_code("C", "
    int max_valid_record = -1;
    int max_valid_state = -1;
    struct record records[NRECORDS];
    struct state states[NSAMPLES];
    pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
    pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
    volatile bool obs_coming = true;
").

%-----------------------------------------------------------------------------%

input_init_obs(_, 0).


input_next_obs(ObsMsg, _, _, I0, I1) :-
    input_next_obs_pure(I0, I1, Ok, Time, AgentS0, Veloc0, Rad0, X0, Y0,
                                          AgentS1, Veloc1, Rad1, X1, Y1),
    (
        Ok = yes,
        % XXX TODO adapt to handle multiple drivers or so
        Agent0 = string_to_agent(AgentS0),
        Agent1 = string_to_agent(AgentS1),
        (   if      I1 = 1
            then    ObsMsg = init_msg([Agent0 - {Veloc0, Rad0, X0, Y0},
                                       Agent1 - {Veloc1, Rad1, X1, Y1}], Time)
            else    ObsMsg = obs_msg({Time, Agent0, X0, Y0, Agent1, X1, Y1})
        )
    ;
        Ok = no,
        ObsMsg = end_of_obs
    ).


:- pred input_next_obs_pure(int::di, int::uo, bool::out, float::out,
    string::out, float::out, float::out, float::out, float::out,
    string::out, float::out, float::out, float::out, float::out) is det.

:- pragma foreign_proc("C",
    input_next_obs_pure(I0::di, I1::uo, Ok::out, T::out,
        Agent0::out, Veloc0::out, Rad0::out, X0::out, Y0::out,
        Agent1::out, Veloc1::out, Rad1::out, X1::out, Y1::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ok = MR_YES;
    while (I0 > max_valid_record) {
        if (pthread_mutex_lock(&mutex) == 0) {
            /* We don't need this condition, but the next observation may be
             * read later.  So we re-test the loop condition and only read
             * (which may block again) only if reading is really necessary. */
            if (I0 > max_valid_record) {
                struct record r;
                int i = scanf(
                        ""%*c %lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\\n"",
                        &r.t,
                        r.agent0, &r.veloc0, &r.rad0, &r.x0, &r.y0,
                        r.agent1, &r.veloc1, &r.rad1, &r.x1, &r.y1);
                if (i == EOF) {
                    Ok = MR_NO;
                    pthread_mutex_unlock(&mutex);
                    break;
                } else if (i == 11) {
                    Ok = MR_YES;
                    push_obs(&r);
                }
            }
            pthread_mutex_unlock(&mutex);
        }
    }
    if (Ok == MR_YES) {
        assert(I0 < NRECORDS);
        T = (MR_Float) records[I0].t;
        Agent0 = MR_make_string_const(records[I0].agent0);
        Veloc0 = (MR_Float) records[I0].veloc0;
        Rad0 = (MR_Float) records[I0].rad0;
        X0 = (MR_Float) records[I0].x0;
        Y0 = (MR_Float) records[I0].y0;
        Agent1 = MR_make_string_const(records[I0].agent1);
        Veloc1 = (MR_Float) records[I0].veloc1;
        Rad1 = (MR_Float) records[I0].rad1;
        X1 = (MR_Float) records[I0].x1;
        Y1 = (MR_Float) records[I0].y1;
        I1 = I0 + 1;
    } else {
        Ok = MR_NO;
        T = (MR_Float) -1.0;
        Agent0 = MR_make_string_const("""");
        Veloc0 = (MR_Float) -1.0;
        Rad0 = (MR_Float) -1.0;
        X0 = (MR_Float) -1.0;
        Y0 = (MR_Float) -1.0;
        Agent1 = MR_make_string_const("""");
        Veloc1 = (MR_Float) -1.0;
        Rad1 = (MR_Float) -1.0;
        X1 = (MR_Float) -1.0;
        Y1 = (MR_Float) -1.0;
    }
").

%-----------------------------------------------------------------------------%

global_init_obs(I, {I1, 0}) :- copy(I, I1).


global_next_obs(ObsMsg, S, P, {ID, I0}, State1) :-
    Done = floor_to_int(reward(S)),
    ToBeDone = match_count(P) - bat.lookahead(S),
    global_next_obs_pure(I0, I1, ID, Done, ToBeDone,
                         Ok, Time, AgentS0, Veloc0, Rad0, X0, Y0,
                             AgentS1, Veloc1, Rad1, X1, Y1),
    (
        Ok = yes,
        % XXX TODO adapt to handle multiple drivers or so
        Agent0 = string_to_agent(AgentS0),
        Agent1 = string_to_agent(AgentS1),
        (   if      I1 = 1
            then    ObsMsg = init_msg([Agent0 - {Veloc0, Rad0, X0, Y0},
                                       Agent1 - {Veloc1, Rad1, X1, Y1}], Time)
            else    ObsMsg = obs_msg({Time, Agent0, X0, Y0, Agent1, X1, Y1})
        )
    ;
        Ok = no,
        ObsMsg = end_of_obs
    ),
    copy({ID, I1}, State1).


:- pred global_next_obs_pure(int::di, int::uo,
    int::in, int::in, int::in,
    bool::out, float::out,
    string::out, float::out, float::out, float::out, float::out,
    string::out, float::out, float::out, float::out, float::out) is det.

:- pragma foreign_proc("C",
    global_next_obs_pure(I0::di, I1::uo,
        ID::in, Done::in, ToBeDone::in,
        Ok::out, T::out,
        Agent0::out, Veloc0::out, Rad0::out, X0::out, Y0::out,
        Agent1::out, Veloc1::out, Rad1::out, X1::out, Y1::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    states[ID] = (struct state) { Done, ToBeDone };
    while (obs_coming && I0 > max_valid_record) {
        printf(""%d && %d > %d\\n"", (int) obs_coming, (int) I0, (int) max_valid_record);
        pthread_cond_wait(&cond, &mutex);
    }
    if (I0 <= max_valid_record) {
        Ok = MR_YES;
        T = (MR_Float) records[I0].t;
        Agent0 = MR_make_string_const(records[I0].agent0);
        Veloc0 = (MR_Float) records[I0].veloc0;
        Rad0 = (MR_Float) records[I0].rad0;
        X0 = (MR_Float) records[I0].x0;
        Y0 = (MR_Float) records[I0].y0;
        Agent1 = MR_make_string_const(records[I0].agent1);
        Veloc1 = (MR_Float) records[I0].veloc1;
        Rad1 = (MR_Float) records[I0].rad1;
        X1 = (MR_Float) records[I0].x1;
        Y1 = (MR_Float) records[I0].y1;
        I1 = I0 + 1;
    } else {
        Ok = MR_NO;
        T = (MR_Float) -1.0;
        Agent0 = MR_make_string_const("""");
        Veloc0 = (MR_Float) -1.0;
        Rad0 = (MR_Float) -1.0;
        X0 = (MR_Float) -1.0;
        Y0 = (MR_Float) -1.0;
        Agent1 = MR_make_string_const("""");
        Veloc1 = (MR_Float) -1.0;
        Rad1 = (MR_Float) -1.0;
        X1 = (MR_Float) -1.0;
        Y1 = (MR_Float) -1.0;
    }
").

%-----------------------------------------------------------------------------%
:- end_module obs.
%-----------------------------------------------------------------------------%

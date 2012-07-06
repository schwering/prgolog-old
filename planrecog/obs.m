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
:- import_module prgolog.
:- import_module io.
:- import_module types.

%-----------------------------------------------------------------------------%

:- type obs_msg ---> init_msg(assoc_list(agent, agent_info), s)
                ;    obs_msg(obs)
                ;    end_of_obs.

:- type init_obs(T) == (pred(int, T)).
:- inst init_obs == (pred(in, uo) is det).

:- type next_obs(A, B, P, T) == (pred(obs_msg, sit(A), prog(A, B, P), T, T)).
:- inst next_obs == (pred(out, in, in, di, uo) is det).

%-----------------------------------------------------------------------------%

:- func append_match(prog(A, B, P), A) = prog(A, B, P) is det <= obs_bat(A, B, P).

:- func remove_match_sequence(prog(A, B, P)) = prog(A, B, P) is semidet <= obs_bat(A, B, P).

:- func last_match(sit(A)) = A is semidet <= obs_bat(A, B, P).

:- pred last_action_covered_by_match(sit(A)::in) is semidet <= obs_bat(A, B, P).

:- func append_obs(prog(A, B, P), obs) = prog(A, B, P) is det <= obs_bat(A, B, P).

:- func match_count_in_prog(prog(A, B, P)) = int <= obs_bat(A, B, P).

:- func match_count_in_sit(sit(A)) = int <= obs_bat(A, B, P).

%-----------------------------------------------------------------------------%

:- pred input_init_obs(int::in, int::uo) is det.

:- pred input_next_obs(obs_msg::out, sit(A)::in, prog(A, B, P)::in,
                       int::di, int::uo) is det.

%-----------------------------------------------------------------------------%

:- pred reset_globals(io::di, io::uo) is det.

:- pred global_init_obs(int::in, {int, int}::uo) is det.

:- pred global_next_obs(obs_msg::out, sit(A)::in, prog(A, B, P)::in,
                        {int, int}::di, {int, int}::uo) is det <= obs_bat(A, B, P).

:- pred mark_observation_end(io::di, io::uo) is det.

:- type activity --->   unused
                    ;   working
                    ;   finished
                    ;   failed.

:- pred update_state(int::in, activity::in, io::di, io::uo) is det.

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


:- func append_match_to_most_right(prog(A, B, P), A) =
                                   prog(A, B, P) is semidet <= obs_bat(A, B, P).

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
    M0 = pseudo_atom(atom(prim(A))),
    is_match_action(A).


remove_match_sequence(conc(P1, P2)) = Q :-
    if          only_match_actions(P2)
    then        Q = P1
    else if     only_match_actions(P1)
    then        Q = P2
    else        false.


:- pred only_match_actions(prog(A, B, P)::in) is semidet <= obs_bat(A, B, P).

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
only_match_actions(pseudo_atom(atom(prim(A)))) :- is_match_action(A).
only_match_actions(nil).


last_match(do(A, S)) =
    ( if is_match_action(A) then A else last_match(S) ).


last_action_covered_by_match(S) :-
    covered_by_match(S).
%    match(_, _, _, T0) = last_match(S),
%    C = (start(S) `=` T0),
%    solve(vargen(S), [C] ++ constraints(S)).


append_obs(P, O) = append_match(P, obs_to_match(O)).


match_count_in_prog(seq(P1, P2)) = match_count_in_prog(P1) + match_count_in_prog(P2).
match_count_in_prog(non_det(P1, P2)) = min(match_count_in_prog(P1), match_count_in_prog(P2)).
match_count_in_prog(conc(P1, P2)) = match_count_in_prog(P1) + match_count_in_prog(P2).
match_count_in_prog(star(_)) = 0.
match_count_in_prog(proc(_)) = 0.
match_count_in_prog(nil) = 0.
match_count_in_prog(pseudo_atom(complex(P))) = match_count_in_prog(P).
match_count_in_prog(pseudo_atom(atom(C))) =
    ( if C = prim(A), is_match_action(A) then 1 else 0 ).


match_count_in_sit(s0) = 0.
match_count_in_sit(do(A, S)) =
    ( if is_match_action(A) then 1 else 0 ) + match_count_in_sit(S).

%-----------------------------------------------------------------------------%


%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include <assert.h>
    #include <pthread.h>
    #include <semaphore.h>
    #include <stdbool.h>

    #include ""obs_types.h""

    #define NRECORDS 1000
    #define NSAMPLES 50

    extern struct record records[];
    extern int max_valid_record;

    extern struct sample_state state_samples[];

    extern sem_t semaphores[];
    extern pthread_mutex_t mutex;

    void push_obs(const struct record *r);
    void read_obs(void);
").


:- pragma foreign_code("C", "
    int max_valid_record;
    struct record records[NRECORDS];
    struct sample_state state_samples[NSAMPLES];
    sem_t semaphores[NSAMPLES];
    pthread_mutex_t mutex;
").


:- pragma foreign_code("C", "
    void push_obs(const struct record *r)
    {
      int i;
      assert(max_valid_record + 1 < NRECORDS);
      memcpy(&records[max_valid_record + 1], r, sizeof(struct record));
      ++max_valid_record;
      for (i = 0; i < NSAMPLES; ++i) {
        sem_post(&semaphores[i]);
      }
    }

    void read_obs(void)
    {
      for (;;) {
        struct record r;
        int i = scanf(
                ""%*c %lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\\n"",
                &r.t,
                r.agent0, &r.veloc0, &r.rad0, &r.x0, &r.y0,
                r.agent1, &r.veloc1, &r.rad1, &r.x1, &r.y1);
        if (i == EOF) {
          int j;
          for (j = 0; i < NSAMPLES; ++i) {
            sem_post(&semaphores[j]);
          }
          break;
        } else if (i == 11) {
          push_obs(&r);
        }
      }
    }
").


:- initialize initialize_globals/2.

:- pred initialize_globals(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    initialize_globals(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int i;
    max_valid_record = -1;
    memset(state_samples, 0, NSAMPLES * sizeof(struct sample_state));
    memset(records, 0, NRECORDS * sizeof(struct record));
    for (i = 0; i < NSAMPLES; ++i) {
        sem_init(&semaphores[i], 0, 0);
    }
    pthread_mutex_init(&mutex, NULL);
    IO1 = IO0;
").


:- finalize finalize_globals/2.

:- pred finalize_globals(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    finalize_globals(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int i;
    for (i = 0; i < NSAMPLES; ++i) {
        sem_destroy(&semaphores[i]);
    }
    pthread_mutex_destroy(&mutex);
    IO1 = IO0;
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
            then    ObsMsg = init_msg(
                        [ Agent0 - agent_info(Veloc0, Rad0, X0, Y0)
                        , Agent1 - agent_info(Veloc1, Rad1, X1, Y1)
                        ], Time)
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

reset_globals(!IO) :-
    finalize_globals(!IO),
    initialize_globals(!IO).


global_init_obs(I, {I1, 0}) :- copy(I, I1).


global_next_obs(ObsMsg, S, P, {ID, I0}, State1) :-
    Done = match_count_in_sit(S),
    ToBeDone = max(0, match_count_in_prog(P) - lookahead(S)),
    global_next_obs_pure(I0, I1, ID, Done, ToBeDone,
                         Ok, Time, AgentS0, Veloc0, Rad0, X0, Y0,
                                   AgentS1, Veloc1, Rad1, X1, Y1),
    (
        Ok = yes,
        % XXX TODO adapt to handle multiple drivers or so
        Agent0 = string_to_agent(AgentS0),
        Agent1 = string_to_agent(AgentS1),
        (   if      I1 = 1
            then    ObsMsg = init_msg(
                        [ Agent0 - agent_info(Veloc0, Rad0, X0, Y0)
                        , Agent1 - agent_info(Veloc1, Rad1, X1, Y1)
                        ], Time)
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
    assert(0 <= ID && ID < NSAMPLES);
    state_samples[ID] = (struct sample_state) { Done, ToBeDone, WORKING };
    sem_wait(&semaphores[ID]);
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


:- pragma foreign_proc("C",
    mark_observation_end(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int i;
    //printf(""BROADCASTING\\n"");
    for (i = 0; i < NSAMPLES; ++i) {
        sem_post(&semaphores[i]);
    }
    IO1 = IO0;
").


update_state(ID, unused, !IO) :- update_state_2(ID, 0, !IO).
update_state(ID, working, !IO) :- update_state_2(ID, 1, !IO).
update_state(ID, finished, !IO) :- update_state_2(ID, 2, !IO).
update_state(ID, failed, !IO) :- update_state_2(ID, 3, !IO).


:- pred update_state_2(int::in, int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    update_state_2(ID::in, Activity::in, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (Activity == 0) state_samples[ID].activity = UNUSED;
    if (Activity == 1) state_samples[ID].activity = WORKING;
    if (Activity == 2) state_samples[ID].activity = FINISHED;
    if (Activity == 3) state_samples[ID].activity = FAILED;
    IO1 = IO0;
").


%-----------------------------------------------------------------------------%
:- end_module obs.
%-----------------------------------------------------------------------------%

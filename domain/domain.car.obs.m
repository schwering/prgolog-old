%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: domain.car.obs.m.
% Main author: schwering.
%
% Types and operations for observations, particularly the generator that reads
% from stdin.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module domain.car.obs.

:- interface.

:- import_module prgolog.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type source.
:- type stream_state.

:- instance obs_source(obs, env, source, stream_state).

:- func source = source.

%-----------------------------------------------------------------------------%

:- pred input_init_obs(int::in, int::uo) is det.

:- pred input_next_obs(obs_msg(obs, env)::out, sit(A)::in, prog(A, B, P)::in,
                       int::di, int::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module domain.obs_util.
:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module solutions.

%-----------------------------------------------------------------------------%

:- type source ---> void.
:- type stream_state ---> ss(stream, int).

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
            then    ObsMsg = init_msg(env(Time,
                        [ Agent0 - agent_info(Veloc0, Rad0, X0, Y0)
                        , Agent1 - agent_info(Veloc1, Rad1, X1, Y1)
                        ]))
            else    ObsMsg = obs_msg(obs(Time, Agent0, X0, Y0, Agent1, X1, Y1))
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

source = void.


:- pred reset_obs_source(source::in, io::di, io::uo) is det.

reset_obs_source(_, !IO) :-
    finalize_globals(!IO),
    initialize_globals(!IO).


:- pred init_obs_stream(source::in, stream::in, stream_state::uo) is det.

init_obs_stream(_, I, ss(I1, 0)) :- copy(I, I1).


:- pred next_obs(obs_msg(obs, env)::out, sit(A)::in, prog(A, B, P)::in,
                 stream_state::di, stream_state::uo)
                 is det <= pr_bat(A, B, P, obs, env).

next_obs(ObsMsg, S, P, ss(ID, I0), State1) :-
    Done = obs_count_in_sit(S),
    ToBeDone = max(0, obs_count_in_prog(P) - lookahead(S)),
    next_obs_pure(I0, I1, ID, Done, ToBeDone,
                         Ok, Time, AgentS0, Veloc0, Rad0, X0, Y0,
                                   AgentS1, Veloc1, Rad1, X1, Y1),
    (
        Ok = yes,
        % XXX TODO adapt to handle multiple drivers or so
        Agent0 = string_to_agent(AgentS0),
        Agent1 = string_to_agent(AgentS1),
        (   if      I1 = 1
            then    ObsMsg = init_msg(env(Time,
                        [ Agent0 - agent_info(Veloc0, Rad0, X0, Y0)
                        , Agent1 - agent_info(Veloc1, Rad1, X1, Y1)
                        ]))
            else    ObsMsg = obs_msg(obs(Time, Agent0, X0, Y0, Agent1, X1, Y1))
        )
    ;
        Ok = no,
        ObsMsg = end_of_obs
    ),
    copy(ss(ID, I1), State1).


:- pred next_obs_pure(
    int::di, int::uo,
    int::in, int::in, int::in,
    bool::out, float::out,
    string::out, float::out, float::out, float::out, float::out,
    string::out, float::out, float::out, float::out, float::out) is det.

:- pragma foreign_proc("C",
    next_obs_pure(
        I0::di, I1::uo,
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


:- pred mark_obs_end(source::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    mark_obs_end(_Src::in, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int i;
    //printf(""BROADCASTING\\n"");
    for (i = 0; i < NSAMPLES; ++i) {
        sem_post(&semaphores[i]);
    }
    IO1 = IO0;
").


:- pred update_state(source::in, stream::in, activity::in, io::di, io::uo)
    is det.

update_state(_, ID, unused, !IO) :- update_state_2(ID, 0, !IO).
update_state(_, ID, working, !IO) :- update_state_2(ID, 1, !IO).
update_state(_, ID, finished, !IO) :- update_state_2(ID, 2, !IO).
update_state(_, ID, failed, !IO) :- update_state_2(ID, 3, !IO).


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

:- instance obs_source(obs, env, source, stream_state) where [
    pred(reset_obs_source/3) is obs.reset_obs_source,
    pred(init_obs_stream/3) is obs.init_obs_stream,
    pred(next_obs/5) is obs.next_obs,
    pred(mark_obs_end/3) is obs.mark_obs_end,
    pred(update_state/5) is obs.update_state
].

%-----------------------------------------------------------------------------%
:- end_module domain.car.obs.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.obs.m.
% Main author: schwering.
%
% Observation interface that reads observations for the car domain from stdin.
%
% Internally, observations are read from stdin and then enqueued in an array for
% subsequent access.
% Reading observations is thread-safe.
%
%-----------------------------------------------------------------------------%

:- module domain.car.obs.torcs.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type source.
:- type stream_state.

:- instance obs_source(obs, env, source, stream_state).

:- func source = source.

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

:- type source ---> void.
:- type stream_state ---> ss(stream, int).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include ""car-obs-torcs-types.h""

    /* Enqueues a new observation.
     * This operation may block. */
    void domain__car__obs__torcs__push_obs(const struct observation_record *obs);

    /* Copies the number of working, finished and failed processes into msg. */
    void init_message(struct planrecog_state *msg);

    /* Computes the current approximated confidence of the plan recognition
     * system. */
    float confidence(void);
").


:- pragma foreign_code("C", "
    #include <assert.h>
    #include <semaphore.h>

    #define MAX_OBSERVATIONS    1000
    #define MAX_PROCESSES       50

    /* The greatest index that points to an initialized observation. */
    static int max_valid_observation;

    /* The array of observations. */
    static struct observation_record observations[MAX_OBSERVATIONS];

    /* States of the sampling processes (number of executed and remaining
     * observations.
     * The semaphores are used for thread-safety. */
    static struct process_state process_states[MAX_PROCESSES];
    static sem_t semaphores[MAX_PROCESSES];
").

%-----------------------------------------------------------------------------%

:- pragma foreign_code("C", "
    void domain__car__obs__torcs__push_obs(const struct observation_record *obs)
    {
      int i;
      assert(max_valid_observation + 1 < MAX_OBSERVATIONS);
      memcpy(&observations[max_valid_observation + 1], obs, sizeof(struct observation_record));
      ++max_valid_observation;
      for (i = 0; i < MAX_PROCESSES; ++i) {
        sem_post(&semaphores[i]);
      }
    }
").


:- pragma foreign_code("C", "
    void init_message(struct planrecog_state *msg) {
        int i;
        msg->working = 0;
        msg->finished = 0;
        msg->failed = 0;
        for (i = 0; i < MAX_PROCESSES; ++i) {
            switch (process_states[i].activity) {
            case UNUSED:                    break;
            case WORKING:  ++msg->working;  break;
            case FINISHED: ++msg->finished; break;
            case FAILED:   ++msg->failed;   break;
            }
        }
    }
").


:- pragma foreign_code("C", "
    float confidence(void) {
        int i;
        float c = 0.0f;
        int n = 0;
        for (i = 0; i < MAX_PROCESSES; ++i) {
            if (process_states[i].activity != UNUSED) {
                if (process_states[i].activity != FAILED &&
                    process_states[i].done + process_states[i].tbd != 0) {
                    c += (double) (process_states[i].done) /
                         (double) (process_states[i].done + process_states[i].tbd);
                }
                ++n;
            }
        }
        return (n > 0) ? (float) c / (float) n : 0.0f;
    }
").

%-----------------------------------------------------------------------------%

:- initialize initialize_globals/2.

:- pred initialize_globals(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    initialize_globals(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int i;
    max_valid_observation = -1;
    memset(process_states, 0, MAX_PROCESSES * sizeof(struct process_state));
    memset(observations, 0, MAX_OBSERVATIONS * sizeof(struct observation_record));
    for (i = 0; i < MAX_PROCESSES; ++i) {
        sem_init(&semaphores[i], 0, 0);
    }
    IO1 = IO0;
").


:- finalize finalize_globals/2.

:- pred finalize_globals(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    finalize_globals(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int i;
    for (i = 0; i < MAX_PROCESSES; ++i) {
        sem_destroy(&semaphores[i]);
    }
    IO1 = IO0;
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
                 stream_state::di, stream_state::uo) is det
                 <= pr_bat(A, B, P, obs, env).

next_obs(ObsMsg, S, P, ss(ID, I0), State1) :-
    Done = obs_count_in_sit(S),
    ToBeDone = int.max(0, int.'-'(obs_count_in_prog(P), lookahead(S))),
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
                        [ pair(Agent0, agent_info(Veloc0, Rad0, X0, Y0))
                        , pair(Agent1, agent_info(Veloc1, Rad1, X1, Y1))
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
    assert(0 <= ID && ID < MAX_PROCESSES);
    process_states[ID] = (struct process_state) { Done, ToBeDone, WORKING };
    sem_wait(&semaphores[ID]);
    if (I0 <= max_valid_observation) {
        Ok = MR_YES;
        T = (MR_Float) observations[I0].t;
        Agent0 = MR_make_string_const(observations[I0].agent0);
        Veloc0 = (MR_Float) observations[I0].veloc0;
        Rad0 = (MR_Float) observations[I0].rad0;
        X0 = (MR_Float) observations[I0].x0;
        Y0 = (MR_Float) observations[I0].y0;
        Agent1 = MR_make_string_const(observations[I0].agent1);
        Veloc1 = (MR_Float) observations[I0].veloc1;
        Rad1 = (MR_Float) observations[I0].rad1;
        X1 = (MR_Float) observations[I0].x1;
        Y1 = (MR_Float) observations[I0].y1;
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
    for (i = 0; i < MAX_PROCESSES; ++i) {
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
    if (Activity == 0) process_states[ID].activity = UNUSED;
    if (Activity == 1) process_states[ID].activity = WORKING;
    if (Activity == 2) process_states[ID].activity = FINISHED;
    if (Activity == 3) process_states[ID].activity = FAILED;
    IO1 = IO0;
").

%-----------------------------------------------------------------------------%

:- instance obs_source(obs, env, source, stream_state) where [
    pred(reset_obs_source/3) is torcs.reset_obs_source,
    pred(init_obs_stream/3) is torcs.init_obs_stream,
    pred(next_obs/5) is torcs.next_obs,
    pred(mark_obs_end/3) is torcs.mark_obs_end,
    pred(update_state/5) is torcs.update_state
].

%-----------------------------------------------------------------------------%
:- end_module domain.car.obs.torcs.
%-----------------------------------------------------------------------------%

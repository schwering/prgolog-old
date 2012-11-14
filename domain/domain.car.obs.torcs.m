%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.obs.torcs.m.
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
    #include <mercury_string.h>
    #include <mercury_tags.h>

    /* Enqueues a new observation.
     * This operation may block. */
    void domain__car__obs__torcs__push_obs(const struct observation_record *obs);

    /* Copies the number of working, finished and failed processes into msg. */
    void domain__car__obs__torcs__init_msg(struct planrecog_state *msg);

    /* Computes the current approximated confidence of the plan recognition
     * system. */
    float confidence(void);
").


:- pragma foreign_decl("C", "
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
    void domain__car__obs__torcs__init_msg(struct planrecog_state *msg) {
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


:- pred init_obs_stream(source::in, stream::in, stream_state::uo,
                        io::di, io::uo) is det.

init_obs_stream(_, I, ss(I1, 0), !IO) :- copy(I, I1).


:- pred next_obs(obs_msg(obs, env)::out, sit(A)::in, prog(A)::in,
                 stream_state::di, stream_state::uo, io::di, io::uo) is det
                 <= pr_bat(A, obs, env).

next_obs(ObsMsg, S, P, ss(ID, I0), State1, !IO) :-
    Done = obs_count_in_sit(S),
    ToBeDone = int.max(0, int.'-'(obs_count_in_prog(P), lookahead(S))),
    next_obs(I0, I1, ID, Done, ToBeDone, Ok, Time, AgentInfoMap, !IO),
    (
        Ok = yes,
        (   if      I1 = 1
            then    ObsMsg = init_msg(env(Time, AgentInfoMap))
            else    ObsMsg = obs_msg(obs(Time, AgentInfoMap))
        )
    ;
        Ok = no,
        ObsMsg = end_of_obs
    ),
    copy(ss(ID, I1), State1).


:- pred next_obs(
    int::di, int::uo, int::in, int::in, int::in,
    bool::out, float::out, assoc_list(agent, info)::out,
    io::di, io::uo) is det.

next_obs(I0, I1, ID, Done, ToBeDone, Ok, T, AgentInfoMap, !IO) :-
    next_obs_pure(I0, I1, ID, Done, ToBeDone, Ok, T,
                  AgentList, VelocList, RadList, XList, YList, !IO),
    (   if      M = merge_lists(AgentList, VelocList, RadList, XList, YList)
        then    AgentInfoMap = M
        else    error("next_obs/8 failed")
    ).


:- func merge_lists(list(string), list(float), list(float), list(float),
                    list(float)) = assoc_list(agent, info).
:- mode merge_lists(in, in, in, in, in) = out is semidet.

merge_lists([], [], [], [], []) = [].
merge_lists([A|As], [V|Vs], [R|Rs], [X|Xs], [Y|Ys]) = [P|Ps] :-
    P = string_to_agent(A) - info(V, R, p(X, Y)),
    Ps = merge_lists(As, Vs, Rs, Xs, Ys).


:- pred next_obs_pure(
    int::di, int::uo,
    int::in, int::in, int::in,
    bool::out, float::out,
    list(string)::out, list(float)::out, list(float)::out,
                       list(float)::out, list(float)::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    next_obs_pure(
        I0::di, I1::uo,
        ID::in, Done::in, ToBeDone::in,
        Ok::out, T::out,
        AgentList::out, VelocList::out, RadList::out, XList::out, YList::out,
        IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    assert(0 <= ID && ID < MAX_PROCESSES);
    process_states[ID] = (struct process_state) { Done, ToBeDone, WORKING };
    sem_wait(&semaphores[ID]);
    if (I0 <= max_valid_observation) {
        int i;
        Ok = MR_YES;
        T = (MR_Float) observations[I0].t;
        AgentList = MR_list_empty();
        VelocList = MR_list_empty();
        RadList   = MR_list_empty();
        XList     = MR_list_empty();
        YList     = MR_list_empty();
        for (i = 0; i < observations[I0].n_agents; ++i) {
            MR_String agent;
            MR_make_aligned_string_copy(agent, observations[I0].info[i].agent);
            AgentList = MR_string_list_cons((MR_Word) agent, AgentList);
            VelocList = MR_list_cons(MR_float_to_word((MR_Float) observations[I0].info[i].veloc), VelocList);
            RadList   = MR_list_cons(MR_float_to_word((MR_Float) observations[I0].info[i].rad),   RadList);
            XList     = MR_list_cons(MR_float_to_word((MR_Float) observations[I0].info[i].x),     XList);
            YList     = MR_list_cons(MR_float_to_word((MR_Float) observations[I0].info[i].y),     YList);
        }
        I1 = I0 + 1;
    } else {
        Ok = MR_NO;
        T = (MR_Float) -1.0;
        AgentList = MR_list_empty();
        VelocList = MR_list_empty();
        RadList   = MR_list_empty();
        XList     = MR_list_empty();
        YList     = MR_list_empty();
    }
    IO1 = IO0;
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
    pred(init_obs_stream/5) is torcs.init_obs_stream,
    pred(next_obs/7) is torcs.next_obs,
    pred(mark_obs_end/3) is torcs.mark_obs_end,
    pred(update_state/5) is torcs.update_state
].

%-----------------------------------------------------------------------------%
:- end_module domain.car.obs.torcs.
%-----------------------------------------------------------------------------%

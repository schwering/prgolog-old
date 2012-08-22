%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.obs.stdin.m.
% Main author: schwering.
%
% Observation interface that reads observations for the car domain from stdin.
%
%-----------------------------------------------------------------------------%

:- module domain.car.obs.stdin.

:- interface.

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
:- type stream_state ---> ss(int).

%-----------------------------------------------------------------------------%

:- pragma foreign_code("C", "
    #include ""car-obs-torcs-types.h""
    #include <assert.h>
    #include <pthread.h>

    #define MAX_OBSERVATIONS    1000

    /* The greatest index that points to an initialized observation. */
    static int max_valid_observation;

    /* The array of observations. */
    static struct observation_record observations[MAX_OBSERVATIONS];

    /* Exclusive access of stdin for thread-safety. */
    static pthread_mutex_t mutex;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_code("C", "
    static void push_obs(const struct observation_record *obs)
    {
      int i;
      assert(max_valid_observation + 1 < MAX_OBSERVATIONS);
      memcpy(&observations[max_valid_observation + 1], obs, sizeof(struct observation_record));
      ++max_valid_observation;
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
    memset(observations, 0, MAX_OBSERVATIONS * sizeof(struct observation_record));
    pthread_mutex_init(&mutex, NULL);
    IO1 = IO0;
").


:- finalize finalize_globals/2.

:- pred finalize_globals(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    finalize_globals(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    pthread_mutex_destroy(&mutex);
    IO1 = IO0;
").

%-----------------------------------------------------------------------------%

source = void.


:- pred init_obs_stream(source::in, stream::in, stream_state::uo) is det.

init_obs_stream(_, _, ss(0)).


:- pred next_obs(obs_msg(obs, env)::out, sit(A)::in, prog(A, B, P)::in,
                 stream_state::di, stream_state::uo) is det
                 <= pr_bat(A, B, P, obs, env).

next_obs(ObsMsg, _, _, ss(I0), State1) :-
    next_obs_pure(I0, I1, Ok, Time, AgentS0, Veloc0, Rad0, X0, Y0,
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
    copy(ss(I1), State1).


:- pred next_obs_pure(int::di, int::uo, bool::out, float::out,
    string::out, float::out, float::out, float::out, float::out,
    string::out, float::out, float::out, float::out, float::out) is det.

:- pragma foreign_proc("C",
    next_obs_pure(I0::di, I1::uo, Ok::out, T::out,
        Agent0::out, Veloc0::out, Rad0::out, X0::out, Y0::out,
        Agent1::out, Veloc1::out, Rad1::out, X1::out, Y1::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ok = MR_YES;
    while (I0 > max_valid_observation) {
        if (pthread_mutex_lock(&mutex) == 0) {
            /* We don't need this condition, but the next observation may be
             * read later.  So we re-test the loop condition and only read
             * (which may block again) only if reading is really necessary. */
            if (I0 > max_valid_observation) {
                struct observation_record r;
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
        assert(I0 < MAX_OBSERVATIONS);
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

%-----------------------------------------------------------------------------%

:- instance obs_source(obs, env, source, stream_state) where [
    (reset_obs_source(_, !IO)),
    pred(init_obs_stream/3) is stdin.init_obs_stream,
    pred(next_obs/5) is stdin.next_obs,
    (mark_obs_end(_, !IO)),
    (update_state(_, _, _, !IO))
].

%-----------------------------------------------------------------------------%
:- end_module domain.car.obs.stdin.
%-----------------------------------------------------------------------------%

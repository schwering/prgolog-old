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

:- instance obs_source(car_obs, source, stream_state).

:- func source = source.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module domain.car.obs.env.
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

:- pragma foreign_decl("C", "
    #include ""domain-car-obs-torcs-types.h""
    #include <assert.h>
    #include <pthread.h>
    #include <stdarg.h>
    #include <stdio.h>

    /* None of these definitions are intended for public use.
     * Observations are read from stdin in this module and enqueued in the
     * array declared below. */

    #define MAX_OBSERVATIONS    1000

    /* The curren stream, stdin by default. */
    static FILE *domain__car__obs__stdin__stream;

    /* The greatest index that points to an initialized observation. Not intended for public use. */
    static int domain__car__obs__stdin__max_valid_observation;

    /* The array of observations. Not intended for public use. */
    static struct observation_record domain__car__obs__stdin__observations[MAX_OBSERVATIONS];

    /* Exclusive access of stdin for thread-safety. Not intended for public use. */
    static pthread_mutex_t domain__car__obs__stdin__mutex;

    /* Enqueues a new observation. Not intended for public use. */
    void domain__car__obs__stdin__push_obs(const struct observation_record *obs);

    /* Gets/sets the curren stream. */
    void domain__car__obs__stdin__set_stream(FILE *fp);
    FILE *domain__car__obs__stdin__get_stream(void);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_code("C", "
    void domain__car__obs__stdin__push_obs(const struct observation_record *obs)
    {
      int i;
      assert(domain__car__obs__stdin__max_valid_observation + 1 < MAX_OBSERVATIONS);
      memcpy(&domain__car__obs__stdin__observations[domain__car__obs__stdin__max_valid_observation + 1], obs, sizeof(struct observation_record));
      ++domain__car__obs__stdin__max_valid_observation;
    }

    void domain__car__obs__stdin__set_stream(FILE *fp)
    {
      domain__car__obs__stdin__stream = fp;
    }

    FILE *domain__car__obs__stdin__get_stream(void)
    {
      return domain__car__obs__stdin__stream;
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
    domain__car__obs__stdin__stream = stdin;
    domain__car__obs__stdin__max_valid_observation = -1;
    memset(domain__car__obs__stdin__observations, 0, MAX_OBSERVATIONS * sizeof(struct observation_record));
    pthread_mutex_init(&domain__car__obs__stdin__mutex, NULL);
    IO1 = IO0;
").


:- finalize finalize_globals/2.

:- pred finalize_globals(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    finalize_globals(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    pthread_mutex_destroy(&domain__car__obs__stdin__mutex);
    IO1 = IO0;
").

%-----------------------------------------------------------------------------%

source = void.


:- pred init_obs_stream(source::in, stream::in, stream_state::uo,
                        io::di, io::uo) is det.

init_obs_stream(_, _, ss(0), !IO).


:- pred next_obs(obs_msg(car_obs)::out, sit(A)::in, prog(A)::in,
                 stream_state::di, stream_state::uo, io::di, io::uo) is det
                 <= pr_bat(A, car_obs).

next_obs(ObsMsg, _, _, ss(I0), State1, !IO) :-
    next_obs2(I0, I1, Ok, Time, AgentInfoMap, !IO),
    (
        Ok = yes,
        (   if      I1 = 1
            then    ObsMsg = init_msg('new car_obs'(env(Time, AgentInfoMap)))
            else    ObsMsg = obs_msg('new car_obs'(env(Time, AgentInfoMap)))
        )
    ;
        Ok = no,
        ObsMsg = end_of_obs
    ),
    copy(ss(I1), State1).



:- pred next_obs2(int::di, int::uo,
                  bool::out, float::out, assoc_list(agent, info)::out,
                  io::di, io::uo) is det.

next_obs2(I0, I1, Ok, T, AgentInfoMap, !IO) :-
    next_obs_pure(I0, I1, Ok, T,
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
    bool::out, float::out,
    list(string)::out, list(float)::out, list(float)::out,
                       list(float)::out, list(float)::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    next_obs_pure(
        I0::di, I1::uo,
        Ok::out, T::out,
        AgentList::out, VelocList::out, RadList::out, XList::out, YList::out,
        IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Ok = MR_YES;
    while (I0 > domain__car__obs__stdin__max_valid_observation) {
        if (pthread_mutex_lock(&domain__car__obs__stdin__mutex) == 0) {
            /* We don't need this condition, but the next observation may be
             * read later.  So we re-test the loop condition and only read
             * (which may block again) only if reading is really necessary. */
            if (I0 > domain__car__obs__stdin__max_valid_observation) {
                struct observation_record r;
                r.n_agents = 2;
                int i = fscanf(domain__car__obs__stdin__stream,
                        ""%*c %lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\\n"",
                        &r.t,
                        r.info[0].agent, &r.info[0].veloc, &r.info[0].rad, &r.info[0].x, &r.info[0].y,
                        r.info[1].agent, &r.info[1].veloc, &r.info[1].rad, &r.info[1].x, &r.info[1].y);
                if (i == EOF) {
                    Ok = MR_NO;
                    pthread_mutex_unlock(&domain__car__obs__stdin__mutex);
                    break;
                } else if (i == 11) {
                    Ok = MR_YES;
                    domain__car__obs__stdin__push_obs(&r);
                }
            }
            pthread_mutex_unlock(&domain__car__obs__stdin__mutex);
        }
    }
    AgentList = MR_list_empty();
    VelocList = MR_list_empty();
    RadList   = MR_list_empty();
    XList     = MR_list_empty();
    YList     = MR_list_empty();
    if (Ok == MR_YES) {
        int i;
        assert(I0 < MAX_OBSERVATIONS);
        T = (MR_Float) domain__car__obs__stdin__observations[I0].t;
        for (i = 0; i < domain__car__obs__stdin__observations[I0].n_agents; ++i) {
            MR_String agent;
            MR_make_aligned_string_copy(agent, domain__car__obs__stdin__observations[I0].info[i].agent);
            AgentList = MR_string_list_cons((MR_Word) agent, AgentList);
            VelocList = MR_list_cons(MR_float_to_word((MR_Float) domain__car__obs__stdin__observations[I0].info[i].veloc), VelocList);
            RadList   = MR_list_cons(MR_float_to_word((MR_Float) domain__car__obs__stdin__observations[I0].info[i].rad),   RadList);
            XList     = MR_list_cons(MR_float_to_word((MR_Float) domain__car__obs__stdin__observations[I0].info[i].x),     XList);
            YList     = MR_list_cons(MR_float_to_word((MR_Float) domain__car__obs__stdin__observations[I0].info[i].y),     YList);
        }
    } else {
        T = (MR_Float) -1.0;
    }
    I1 = I0 + 1;
    IO1 = IO0;
").

%-----------------------------------------------------------------------------%

:- instance obs_source(car_obs, source, stream_state) where [
    (reset_obs_source(_, !IO)),
    pred(init_obs_stream/5) is stdin.init_obs_stream,
    pred(next_obs/7) is stdin.next_obs,
    (mark_obs_end(_, !IO)),
    (update_state(_, _, _, !IO))
].

%-----------------------------------------------------------------------------%
:- end_module domain.car.obs.stdin.
%-----------------------------------------------------------------------------%

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
% The root of the hierarchy are the sources. Actually all sources access the
% very same observations. But sources have independent copies of the observation
% queue. Thus they may reset_obs_source/3 its queue, for example.
% Furthermore one might create one source per hypothesis. Each hypothesis'
% confidence can be acquired separately using the function confidence/1.
%
% Note that if you use the TORCS observation interface repeatedly in otherwise
% independent iterations, you should cal reset_all_sources/2 after each
% iteration. Otherwise you might quickly run out of sources, because the counter
% is continually incremented.
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

:- pred new_source(source::uo, io::di, io::uo) is det.
:- pred reset_all_sources(io::di, io::uo) is det.

:- func min_confidence(source) = float.
:- mode min_confidence(ui) = out is det.
:- mode min_confidence(in) = out is det.

:- func max_confidence(source) = float.
:- mode max_confidence(ui) = out is det.
:- mode max_confidence(in) = out is det.

:- func confidences(source) = {float, float}.
:- mode confidences(ui) = out is det.
:- mode confidences(in) = out is det.

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

:- type source ---> s(int).
:- type stream_state ---> ss(source, stream, int).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include ""domain-car-obs-torcs-types.h""
    #include <mercury_string.h>
    #include <mercury_tags.h>

    /* Enqueues a new observation.
     * This operation may block. */
    void domain__car__obs__torcs__push_obs(const struct observation_record *obs);

    /* Copies the number of working, finished and failed processes into msg. */
    void domain__car__obs__torcs__init_msg(struct planrecog_state *msg);
").


:- pragma foreign_decl("C", "
    #include <assert.h>
    #include <semaphore.h>

    #define MAX_OBSERVATIONS    1000
    #define MAX_SOURCES         8
    #define MAX_STREAMS         16

    /* Minor states a sample process can be in. */
    enum activity {
      UNUSED = 0,   /* initially the process does nothing */
      WORKING = 1,  /* then the process incrementally executes the program */
      FINISHED = 2, /* finally the process either completes execution */
      FAILED = 3    /* or execution fails */
    };


    /* The current sources. The structure follows the source-stream hierarchy. */
    static int max_valid_source;    /* next source id */
    static struct {
        int max_valid_observation;  /* greatest index that points to an initialized observation */
        struct observation_record observations[MAX_OBSERVATIONS]; /* the array of observations. */
        int max_valid_stream;       /* next valid stream */
        struct {
            sem_t sem;              /* contains the number of enqueued observations */
            int done;               /* observations matched until now */
            int tbd;                /* observations still to matched */
            enum activity activity; /* is it working, has it succeeded or failed? */
        } streams[MAX_STREAMS];
    } sources[MAX_SOURCES];

    /* Computes the current approximated confidence of the plan recognition
     * system. */
    static float min_confidence(int source);
    static float max_confidence(int source);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_code("C", "
    void domain__car__obs__torcs__push_obs(const struct observation_record *obs)
    {
        int i, j;
        for (i = 0; i < MAX_SOURCES; ++i) {
            const int k = ++sources[i].max_valid_observation;
            assert(k < MAX_OBSERVATIONS);
            memcpy(&sources[i].observations[k], obs, sizeof(*obs));
            for (j = 0; j < MAX_STREAMS; ++j) {
                sem_post(&sources[i].streams[j].sem);
            }
        }
    }
").


:- pragma foreign_code("C", "
    void domain__car__obs__torcs__init_msg(struct planrecog_state *msg) {
        int i, j;
        memset(msg, 0, sizeof(*msg));
        assert(max_valid_source < NSOURCES);
        msg->n_sources = max_valid_source + 1;
        for (i = 0; i <= max_valid_source; ++i) {
            for (j = 0; j <= sources[i].max_valid_stream; ++j) {
                switch (sources[i].streams[j].activity) {
                    case UNUSED:                               break;
                    case WORKING:  ++msg->sources[i].working;  break;
                    case FINISHED: ++msg->sources[i].finished; break;
                    case FAILED:   ++msg->sources[i].failed;   break;
                }
            }
        }
    }
").


:- pragma foreign_code("C", "
    static float min_confidence(int source) {
        struct planrecog_state msg;
        int numer, denom;
        domain__car__obs__torcs__init_msg(&msg);
        numer = msg.sources[source].finished;
        denom = msg.sources[source].working +
                msg.sources[source].finished +
                msg.sources[source].failed;                              
        return (float) numer / (float) denom;
    }
").


:- pragma foreign_code("C", "
    static float max_confidence(int source) {
        struct planrecog_state msg;
        int numer, denom;
        domain__car__obs__torcs__init_msg(&msg);
        numer = msg.sources[source].working +
                msg.sources[source].finished;
        denom = msg.sources[source].working +
                msg.sources[source].finished +
                msg.sources[source].failed;                              
        return (float) numer / (float) denom;
    }
").


/* XXX this function is currently not used. What does it actually compute?! 
 * I guess it's the confidence where each stream which is still working is weighted with
 * its quotient of processed and total observations. */
:- pragma foreign_code("C", "
    static float confidence(int source) {
        int j;
        float c = 0.0f;
        int n = 0;
        assert(0 <= source && source <= max_valid_source);
        for (j = 0; j <= sources[source].max_valid_stream; ++j) {
            if (sources[source].streams[j].activity != UNUSED) {
                if (sources[source].streams[j].activity != FAILED &&
                    sources[source].streams[j].done + sources[source].streams[j].tbd != 0) {
                    c += (double) (sources[source].streams[j].done) /
                         (double) (sources[source].streams[j].done + sources[source].streams[j].tbd);
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
    int i, j;
    memset(&sources, 0, sizeof(sources));
    max_valid_source = -1;
    for (i = 0; i < MAX_SOURCES; ++i) {
        sources[i].max_valid_observation = -1;
        sources[i].max_valid_stream = -1;
        for (j = 0; j < MAX_STREAMS; ++j) {
            sem_init(&sources[i].streams[j].sem, 0, 0);
        }
    }
    IO1 = IO0;
").


:- finalize finalize_globals/2.

:- pred finalize_globals(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    finalize_globals(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int i, j;
    for (i = 0; i < MAX_SOURCES; ++i) {
        for (j = 0; j < MAX_STREAMS; ++j) {
            sem_destroy(&sources[i].streams[j].sem);
        }
    }
    IO1 = IO0;
").

%-----------------------------------------------------------------------------%

new_source(s(Source), !IO) :- new_source_2(Source, !IO).


:- pred new_source_2(int::uo, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    new_source_2(Source::uo, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int j;
    Source = (++max_valid_source);
    assert(0 <= Source && Source < MAX_SOURCES);
    memset(&sources[Source], 0, sizeof(sources[Source]));
    sources[Source].max_valid_observation = -1;
    sources[Source].max_valid_stream = -1;
    IO1 = IO0;
").


reset_all_sources(!IO) :-
    finalize_globals(!IO),
    initialize_globals(!IO).


:- pred reset_obs_source(source::in, io::di, io::uo) is det.

reset_obs_source(s(Source), !IO) :- reset_obs_source_2(Source, !IO).


:- pred reset_obs_source_2(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    reset_obs_source_2(Source::in, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int j;
    assert(0 <= Source && Source < MAX_SOURCES);
    for (j = 0; j < MAX_STREAMS; ++j) {
        sem_destroy(&sources[Source].streams[j].sem);
    }
    memset(&sources[Source], 0, sizeof(sources[Source]));
    sources[Source].max_valid_observation = -1;
    sources[Source].max_valid_stream = -1;
    for (j = 0; j < MAX_STREAMS; ++j) {
        sem_init(&sources[Source].streams[j].sem, 0, 0);
    }
    IO1 = IO0;
").


confidences(Source) = {min_confidence(Source), max_confidence(Source)}.


max_confidence(Source) = max_confidence_2(I) :- copy(Source, s(I)).


:- func max_confidence_2(int::in) = (float::out) is det.

:- pragma foreign_proc("C",
    max_confidence_2(Source::in) = (Conf::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Conf = (MR_Float) max_confidence(Source);
").


min_confidence(Source) = min_confidence_2(I) :- copy(Source, s(I)).


:- func min_confidence_2(int::in) = (float::out) is det.

:- pragma foreign_proc("C",
    min_confidence_2(Source::in) = (Conf::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Conf = (MR_Float) min_confidence(Source);
").


:- pred init_obs_stream(source::in, stream::in, stream_state::uo,
                        io::di, io::uo) is det.

init_obs_stream(Source, Stream, ss(Source1, Stream1, 0), !IO) :-
    copy(Source, Source1),
    copy(Stream, Stream1),
    Source = s(SourceID),
    init_obs_stream_2(SourceID, Stream, !IO).


:- pred init_obs_stream_2(int::in, int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    init_obs_stream_2(Source::in, Stream::in, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (sources[Source].max_valid_stream < Stream) {
        sources[Source].max_valid_stream = Stream;
    }
    IO1 = IO0;
").


:- pred next_obs(obs_msg(obs, env)::out, sit(A)::in, prog(A)::in,
                 stream_state::di, stream_state::uo, io::di, io::uo) is det
                 <= pr_bat(A, obs, env).

next_obs(ObsMsg, S, P, ss(Source, Stream, I0), State1, !IO) :-
    Done = obs_count_in_sit(S),
    ToBeDone = int.max(0, int.'-'(obs_count_in_prog(P), lookahead(S))),
    next_obs(I0, I1, Source, Stream, Done, ToBeDone, Ok, Time, AgentInfoMap, !IO),
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
    copy(ss(Source, Stream, I1), State1).


:- pred next_obs(
    int::di, int::uo,
    source::in, stream::in,
    int::in, int::in,
    bool::out, float::out, assoc_list(agent, info)::out,
    io::di, io::uo) is det.

next_obs(I0, I1, s(Source), Stream, Done, ToBeDone, Ok, T, AgentInfoMap, !IO) :-
    next_obs_pure(I0, I1, Source, Stream, Done, ToBeDone, Ok, T,
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
    int::in, int::in,
    int::in, int::in,
    bool::out, float::out,
    list(string)::out, list(float)::out, list(float)::out,
                       list(float)::out, list(float)::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    next_obs_pure(
        I0::di, I1::uo,
        Source::in, Stream::in,
        Done::in, ToBeDone::in,
        Ok::out, T::out,
        AgentList::out, VelocList::out, RadList::out, XList::out, YList::out,
        IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    assert(0 <= Source && Source < MAX_SOURCES);
    assert(0 <= Stream && Stream < MAX_STREAMS);
    sources[Source].streams[Stream].done = Done;
    sources[Source].streams[Stream].tbd = ToBeDone;
    sources[Source].streams[Stream].activity = WORKING;
    sem_wait(&sources[Source].streams[Stream].sem);
    if (I0 <= sources[Source].max_valid_observation) {
        int i;
        Ok = MR_YES;
        T = (MR_Float) sources[Source].observations[I0].t;
        AgentList = MR_list_empty();
        VelocList = MR_list_empty();
        RadList   = MR_list_empty();
        XList     = MR_list_empty();
        YList     = MR_list_empty();
        for (i = 0; i < sources[Source].observations[I0].n_agents; ++i) {
            MR_String agent;
            MR_make_aligned_string_copy(agent, sources[Source].observations[I0].info[i].agent);
            AgentList = MR_string_list_cons((MR_Word) agent, AgentList);
            VelocList = MR_list_cons(MR_float_to_word((MR_Float) sources[Source].observations[I0].info[i].veloc), VelocList);
            RadList   = MR_list_cons(MR_float_to_word((MR_Float) sources[Source].observations[I0].info[i].rad),   RadList);
            XList     = MR_list_cons(MR_float_to_word((MR_Float) sources[Source].observations[I0].info[i].x),     XList);
            YList     = MR_list_cons(MR_float_to_word((MR_Float) sources[Source].observations[I0].info[i].y),     YList);
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

mark_obs_end(s(Source), !IO) :- mark_obs_end_2(Source, !IO).


:- pred mark_obs_end_2(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    mark_obs_end_2(_Src::in, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int i, j;
    for (i = 0; i < MAX_SOURCES; ++i) {
        for (j = 0; j < MAX_STREAMS; ++j) {
            sem_post(&sources[i].streams[j].sem);
        }
    }
    IO1 = IO0;
").


:- pred update_state(source::in, stream::in, activity::in, io::di, io::uo)
    is det.

update_state(s(Source), Stream, Activity, !IO) :-
    (   Activity = unused,   I = 0
    ;   Activity = working,  I = 1
    ;   Activity = finished, I = 2
    ;   Activity = failed,   I = 3
    ),
    update_state_2(Source, Stream, I, !IO).


:- pred update_state_2(int::in, int::in, int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    update_state_2(Source::in, Stream::in, Activity::in, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    assert(0 <= Source && Source <= max_valid_source);
    assert(0 <= Stream && Stream <= sources[Source].max_valid_stream);
    if (Activity == 0) sources[Source].streams[Stream].activity = UNUSED;
    if (Activity == 1) sources[Source].streams[Stream].activity = WORKING;
    if (Activity == 2) sources[Source].streams[Stream].activity = FINISHED;
    if (Activity == 3) sources[Source].streams[Stream].activity = FAILED;
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

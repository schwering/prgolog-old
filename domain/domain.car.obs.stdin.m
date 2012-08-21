%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: domain.car.obs.stdin.m.
% Main author: schwering.
%
% TODO description outdated
% Types and operations for observations, particularly the generator that reads
% from stdin.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module domain.car.obs.stdin.

:- interface.

%-----------------------------------------------------------------------------%

:- type source.
:- type stream_state.

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

:- pred input_init_obs(int::in, int::uo) is det.

:- pred input_next_obs(obs_msg(obs, env)::out, sit(A)::in, prog(A, B, P)::in,
                       int::di, int::uo) is det.

%-----------------------------------------------------------------------------%

source = void.


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
:- end_module domain.car.obs.stdin.
%-----------------------------------------------------------------------------%

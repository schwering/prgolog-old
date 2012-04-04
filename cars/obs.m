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
:- import_module prgolog.
:- import_module types.

%-----------------------------------------------------------------------------%

:- type obs_msg ---> init_msg(assoc_list(agent, {mps, rad, m, m}), s)
                ;    obs_msg(obs)
                ;    end_of_obs.

:- type init_obs(T) == (pred(T)).
:- inst init_obs == (pred(uo) is det).
:- type next_obs(T) == (pred(obs_msg, T, T)).
:- inst next_obs == (pred(out, di, uo) is det).

%-----------------------------------------------------------------------------%

:- func append_match(prog(prim, stoch, proc), prim) =
    prog(prim, stoch, proc) is det.

:- func remove_match_sequence(prog(prim, stoch, proc)) =
    prog(prim, stoch, proc) is semidet.

:- func last_match(sit(prim)) = prim is semidet.

:- pred last_action_covered_by_match(sit(prim)::in) is semidet.

:- func append_obs(prog(prim, stoch, proc), obs) =
    prog(prim, stoch, proc) is det.

%-----------------------------------------------------------------------------%

:- pred input_init_obs(int::uo) is det.
:- pred input_next_obs(obs_msg::out, int::di, int::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module prgolog.ccfluent.
:- import_module solutions.

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%

append_match(P, O) =
    (   if      P1 = append_match_to_most_right(P, O)
        then    P1
        else    conc(P, pseudo_atom(atom(prim(O))))
    ).


:- func append_match_to_most_right(prog(prim, stoch, proc), prim) =
                                   prog(prim, stoch, proc) is semidet.

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


:- pred only_match_actions(prog(prim, stoch, proc)::in) is semidet.

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

input_init_obs(0).


input_next_obs(ObsMsg, I0, I1) :-
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


:- pragma foreign_decl("C", "
    #define NRECORDS 500
    #define AGENTLEN 15
    struct record {
        double t;
        char agent0[AGENTLEN+1];
        double veloc0;
        double rad0;
        double x0;
        double y0;
        char agent1[AGENTLEN+1];
        double veloc1;
        double rad1;
        double x1;
        double y1;
    };
").


:- pragma foreign_code("C", "
    volatile int max_valid_record = -1;
    struct record records[NRECORDS];
    pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
").


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
                    memcpy(&records[max_valid_record+1], &r,
                        sizeof(struct record));
                    ++max_valid_record;
                }
            }
            pthread_mutex_unlock(&mutex);
        }
    }
    if (Ok == MR_YES) {
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

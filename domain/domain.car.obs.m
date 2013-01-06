%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.obs.m.
% Main author: schwering.
%
% Types and operations for observations in the car domain.
%
%-----------------------------------------------------------------------------%

:- module domain.car.obs.

:- interface.

:- import_module prgolog.
:- import_module io.

%-----------------------------------------------------------------------------%

:- pragma obsolete(remove_obs_sequence/1).
:- func remove_obs_sequence(prog(A)) = prog(A) is semidet <= obs_bat(A, O).

    % Does not substitute anything in called procedures!
:- func map_prog(pred(prog(A), prog(A)), prog(A)) = prog(A) <= obs_bat(A, O).
:- mode map_prog(pred(in, out) is semidet, in) = out is det.

    % Does not substitute observations in called procedures!
:- func subst_obs(prog(A), prog(A)) = prog(A) is det <= obs_bat(A, O).

:- func last_obs(sit(A)) = A is semidet <= obs_bat(A, O).

:- pred last_action_covered_by_obs(sit(A)::in) is semidet <= obs_bat(A, O).

:- func append_obs(prog(A), O) = prog(A) is det <= obs_bat(A, O).

:- func obs_count_in_prog(prog(A)) = int <= obs_bat(A, O).

:- func obs_count_in_sit(sit(A)) = int <= obs_bat(A, O).

%-----------------------------------------------------------------------------%

:- include_module torcs.
:- include_module stdin.

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

:- func append_obs_to_most_right(prog(A), pseudo_atom(A)) = prog(A) is semidet
    <= obs_bat(A, O).

append_obs_to_most_right(seq(P1, P2), PA) =
    (   if      Q2 = append_obs_to_most_right(P2, PA)
        then    seq(P1, Q2)
        else    append_obs_to_most_right(P1, PA) ).
append_obs_to_most_right(non_det(P1, P2), PA) =
    (   if      Q2 = append_obs_to_most_right(P2, PA)
        then    non_det(P1, Q2)
        else    append_obs_to_most_right(P1, PA) ).
append_obs_to_most_right(conc(P1, P2), PA) =
    (   if      Q2 = append_obs_to_most_right(P2, PA)
        then    conc(P1, Q2)
        else    append_obs_to_most_right(P1, PA) ).
append_obs_to_most_right(pseudo_atom(PA1), PA2) =
    seq(pseudo_atom(PA1), pseudo_atom(PA2)) :- is_obs_prog(PA1).


    % This is broken!!!
    % It fails for the program seq(nil, conc(nil, nil))
    % because of the seq(nil, _) part.
remove_obs_sequence(conc(P1, P2)) = Q :-
    if          only_obs_actions(P2)
    then        Q = P1
    else if     only_obs_actions(P1)
    then        Q = P2
    else        false.


map_prog(Pred, P) = Q :-
    if Pred(P, Q1) then Q = Q1 else
    (   P = seq(P1, P2),     Q = seq(map_prog(Pred, P1),
                                     map_prog(Pred, P2))
    ;   P = non_det(P1, P2), Q = non_det(map_prog(Pred, P1),
                                         map_prog(Pred, P2))
    ;   P = conc(P1, P2),    Q = conc(map_prog(Pred, P1),
                                      map_prog(Pred, P2))
    ;   P = pick(F, X0, P1), Q = 'new pick'(F, X0,
                                            func(X) = map_prog(Pred, P1(X)))
    ;   P = star(P1),        Q = star(map_prog(Pred, P1))
    ;   P = proc(_),         Q = P
    ;   P = pseudo_atom(_),  Q = P
    ;   P = nil,             Q = P
    ).
    

subst_obs(New, P) = map_prog(Pred, P) :-
    Pred = (pred(pseudo_atom(PA)::in, New1::out) is semidet :-
        is_obs_prog(PA),
        New1 = New
    ).


:- pred only_obs_actions(prog(A)::in) is semidet <= obs_bat(A, O).

only_obs_actions(seq(P1, P2))     :- only_obs_actions(P1), only_obs_actions(P2).
only_obs_actions(non_det(P1, P2)) :- only_obs_actions(P1), only_obs_actions(P2).
only_obs_actions(conc(P1, P2))    :- only_obs_actions(P1), only_obs_actions(P2).
only_obs_actions(star(P))         :- only_obs_actions(P).
only_obs_actions(pseudo_atom(PA)) :- is_obs_prog(PA).
only_obs_actions(nil).


last_obs(do(A, S)) = ( if is_obs_action(A) then A else last_obs(S) ).


last_action_covered_by_obs(S) :-
    covered_by_obs(S).
%    obs(_, _, _, T0) = last_obs(S),
%    C = (start(S) `=` T0),
%    solve(vargen(S), [C] ++ constraints(S)).


append_obs(P, O) = P2 :-
    PA = obs_to_action(O),
    (   if      P1 = append_obs_to_most_right(P, PA)
        then    P2 = P1
        else    P2 = conc(P, pseudo_atom(PA))
    ).


obs_count_in_prog(seq(P1, P2)) = obs_count_in_prog(P1) + obs_count_in_prog(P2).
obs_count_in_prog(non_det(P1, P2)) = min(obs_count_in_prog(P1), obs_count_in_prog(P2)).
obs_count_in_prog(conc(P1, P2)) = obs_count_in_prog(P1) + obs_count_in_prog(P2).
obs_count_in_prog(pick(_, _, _)) = 0.
obs_count_in_prog(star(_)) = 0.
obs_count_in_prog(proc(_)) = 0.
obs_count_in_prog(nil) = 0.
obs_count_in_prog(pseudo_atom(PA @ complex(P))) =
    ( if is_obs_prog(PA) then 1 else obs_count_in_prog(P) ).
obs_count_in_prog(pseudo_atom(PA @ atom(_))) =
    ( if is_obs_prog(PA) then 1 else 0 ).


obs_count_in_sit(s0) = 0.
obs_count_in_sit(do(A, S)) =
    ( if is_obs_action(A) then 1 else 0 ) + obs_count_in_sit(S).

%-----------------------------------------------------------------------------%
:- end_module domain.car.obs.
%-----------------------------------------------------------------------------%

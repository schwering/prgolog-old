%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: domain.obs_util.m.
% Main author: schwering.
%
% Generic types and operations for observations.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module domain.obs_util.

:- interface.

:- import_module prgolog.

%-----------------------------------------------------------------------------%

:- func remove_obs_sequence(prog(A, B, P)) = prog(A, B, P) is semidet
    <= obs_bat(A, B, P, O).

:- func last_obs(sit(A)) = A is semidet
    <= obs_bat(A, B, P, O).

:- pred last_action_covered_by_obs(sit(A)::in) is semidet
    <= obs_bat(A, B, P, O).

:- func append_obs(prog(A, B, P), O) = prog(A, B, P) is det
    <= obs_bat(A, B, P, O).

:- func obs_count_in_prog(prog(A, B, P)) = int
    <= obs_bat(A, B, P, O).

:- func obs_count_in_sit(sit(A)) = int
    <= obs_bat(A, B, P, O).

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

:- func append_obs_to_most_right(prog(A, B, P), A) = prog(A, B, P) is semidet
    <= obs_bat(A, B, P, O).

append_obs_to_most_right(seq(P1, P2), M) =
    (   if      Q2 = append_obs_to_most_right(P2, M)
        then    seq(P1, Q2)
        else    append_obs_to_most_right(P1, M) ).
append_obs_to_most_right(non_det(P1, P2), M) =
    (   if      Q2 = append_obs_to_most_right(P2, M)
        then    non_det(P1, Q2)
        else    append_obs_to_most_right(P1, M) ).
append_obs_to_most_right(conc(P1, P2), M) =
    (   if      Q2 = append_obs_to_most_right(P2, M)
        then    conc(P1, Q2)
        else    append_obs_to_most_right(P1, M) ).
append_obs_to_most_right(star(P), M) =
    append_obs_to_most_right(P, M).
append_obs_to_most_right(M0, M) = seq(M0, pseudo_atom(atom(prim(M)))) :-
    M0 = pseudo_atom(atom(prim(A))),
    is_obs(A).


remove_obs_sequence(conc(P1, P2)) = Q :-
    if          only_obs_actions(P2)
    then        Q = P1
    else if     only_obs_actions(P1)
    then        Q = P2
    else        false.


:- pred only_obs_actions(prog(A, B, P)::in) is semidet
    <= obs_bat(A, B, P, O).

only_obs_actions(seq(P1, P2)) :-
    only_obs_actions(P1),
    only_obs_actions(P2).
only_obs_actions(non_det(P1, P2)) :-
    only_obs_actions(P1),
    only_obs_actions(P2).
only_obs_actions(conc(P1, P2)) :-
    only_obs_actions(P1),
    only_obs_actions(P2).
only_obs_actions(star(P)) :-
    only_obs_actions(P).
only_obs_actions(pseudo_atom(atom(prim(A)))) :- is_obs(A).
only_obs_actions(nil).


last_obs(do(A, S)) =
    ( if is_obs(A) then A else last_obs(S) ).


last_action_covered_by_obs(S) :-
    covered_by_obs(S).
%    obs(_, _, _, T0) = last_obs(S),
%    C = (start(S) `=` T0),
%    solve(vargen(S), [C] ++ constraints(S)).


append_obs(P, O) = P2 :-
    A = obs_to_action(O),
    (   if      P1 = append_obs_to_most_right(P, A)
        then    P2 = P1
        else    P2 = conc(P, pseudo_atom(atom(prim(A))))
    ).


obs_count_in_prog(seq(P1, P2)) = obs_count_in_prog(P1) + obs_count_in_prog(P2).
obs_count_in_prog(non_det(P1, P2)) = min(obs_count_in_prog(P1), obs_count_in_prog(P2)).
obs_count_in_prog(conc(P1, P2)) = obs_count_in_prog(P1) + obs_count_in_prog(P2).
obs_count_in_prog(star(_)) = 0.
obs_count_in_prog(proc(_)) = 0.
obs_count_in_prog(nil) = 0.
obs_count_in_prog(pseudo_atom(complex(P))) = obs_count_in_prog(P).
obs_count_in_prog(pseudo_atom(atom(C))) =
    ( if C = prim(A), is_obs(A) then 1 else 0 ).


obs_count_in_sit(s0) = 0.
obs_count_in_sit(do(A, S)) =
    ( if is_obs(A) then 1 else 0 ) + obs_count_in_sit(S).

%-----------------------------------------------------------------------------%
:- end_module domain.obs_util.
%-----------------------------------------------------------------------------%

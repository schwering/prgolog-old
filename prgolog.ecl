% vim:filetype=prolog:textwidth=80:shiftwidth=4:softtabstop=4:expandtab
%
% This is a Golog interpreter written in ECLiPSe-CLP.
% The idea is to compare the performance of Mercury to ECLiPSe-CLP.
%
% It is based on
% * the interpreter I wrote for my diploma thesis in ECLiPSe-CLP and
% * the one I implemented in Mercury.
%
% I took the first one and stripped away the time and continuous change-specific
% parts, so that its functionality matches the Mercury implementation.
%
% Christoph Schwering (schwering@gmail.com)

:- module(prgolog).

:- use_module(worlds).
:- use_module(library(apply)).

:- export(next/2).
:- export(next2/2).
:- export(maybe_final/1).
:- export(trans_atom/3).
:- export(trans/7).
:- export(final/4).
:- export(reward/3).
:- export(trans_max_h/7).
:- export(do/4).

:- export(holds/2).
:- export(ssa/3).

:- export op(700, xfy, in).  /* Interval */
:- export op(800, xfy, &).   /* Conjunction */
:- export op(850, xfy, v).   /* Disjunction */
:- export op(870, xfy, =>).  /* Implication */
:- export op(880, xfy, <=>). /* Equivalence */
:- export op(1100, xfy, ;).   /* Action sequence */
:- export op(1150, xfy, #).   /* Nondeterministic action choice */
:- export op(1175, xfy, \\).  /* Interleaved execution */


next(E, Ps) :- var(E), !, error(2, next(E, Ps)).
next(nil, []) :- !.
next(A, Ps) :- \+ \+ primitive_action(A), !, Ps = [(A, nil)].
next(B, Ps) :- \+ \+ stochastic_action(B), !, Ps = [(B, nil)].
next(T, Ps) :- \+ \+ T = ?(_), !, Ps = [(T, nil)].
next(E, Ps) :- \+ \+ E = atomic(_), !, Ps = [(E, nil)].
next(E, Ps) :- proc(E, E0), !, next(E0, Ps).
next(pi(V, E), Ps) :- !, pi(V, E, E1), next(E1, Ps).
next(EA # EB, Ps) :- !, next(EA, Ps0), next(EB, Ps1), append(Ps0, Ps1, Ps).
next(star(E), Ps) :-
    !, next(E, Ps0),
    ( param(E), foreach((C, R1), Ps0), foreach((C, R), Ps) do R = (R1 ; star(E)) ).
next(EA ; EB, Ps) :-
    !, next(EA, Ps0),
    ( param(EB), foreach((C, R1), Ps0), foreach((C, R), Ps1) do R = (R1 ; EB) ),
    ( maybe_final(EA) -> next(EB, Ps2) ; Ps2 = [] ),
    append(Ps1, Ps2, Ps).
next(EA \\ EB, Ps) :-
    !, next(EA, Ps0),
    ( param(EB), foreach((C, R1), Ps0), foreach((C, R), Ps1) do R = (R1 \\ EB) ),
    next(EB, Ps2),
    ( param(EA), foreach((C, R2), Ps2), foreach((C, R), Ps3) do R = (EA \\ R2) ),
    append(Ps1, Ps3, Ps).
next(E, Ps) :- error(2, next(E, Ps)).


next2(E, Ps) :-
    next(E, PsComplex),
    ( foreach((C, R), PsComplex), fromto([], Ps0, Ps1, Ps) do
        ( C = atomic(Complex) ->
            next2(Complex ; R, PsSub)
        ;
            PsSub = [(C, R)]
        ),
        append(PsSub, Ps0, Ps1)
    ).


maybe_final(nil).
maybe_final(A) :- \+ \+ primitive_action(A), !, fail.
maybe_final(B) :- \+ \+ stochastic_action(B), !, fail.
maybe_final(T) :- \+ \+ T = ?(_), !, fail.
maybe_final(E) :- \+ \+ E = atomic(_), !, fail.
maybe_final(pi(V, E)) :- pi(V, E, E1), maybe_final(E1).
maybe_final(EA # EB) :- once (maybe_final(EA) ; maybe_final(EB)).
maybe_final(star(_)).
maybe_final(EA ; EB) :- maybe_final(EA), maybe_final(EB).
maybe_final(EA \\ EB) :- maybe_final(EA), maybe_final(EB).


trans_atom(A, S, S1) :-
    \+ \+ primitive_action(A), !,
    poss(A, S),
    S1 = do(A, S).
trans_atom(B, S, S1) :-
    \+ \+ stochastic_action(B), !,
    random_outcome(B, S, A),
    trans_atom(A, S, S1).
trans_atom(E, S, S1) :-
    E = ?(G), !,
    holds(G, S),
    S1 = S.


trans(RewardF, H, E, S, H1, E1, S1) :-
    H < 0, !,
    error(6, trans(RewardF, H, E, S, H1, E1, S1)).
trans(RewardF, H, E, S, H1, E1, S1) :-
    H > 0,
    next2(E, Ps),
    ( Ps = [] ->
        fail
    ; Ps = [(C, E1)] ->
        trans_atom(C, S, S1),
        H1 is new_horizon(H, C)
    ;
        ( param(RewardF, H, S),
          foreach((C, E), Ps),
          fromto((0, nil, nullsit), (H0, E0, S0), (H1, E1, S1), (H1, E1, S1)),
          fromto((0, 0),            (LR0, LM0),   (LR1, LM1),   _) do
            (   trans_atom(C, S, LS1),
                H2 is new_horizon(H, C),
                trans_max_h(RewardF, H2, E, LS1, _LE2, LS2, LM2),
                LR2 is reward(RewardF, LS2),
                (   S0 == nullsit
                ;   LR2 > LR0
                ;   LR2 =:= LR0, LM2 > LM0)
            ->
                (H1, E1, S1) = (H2, E, LS1),
                (LR1, LM1)   = (LR2, LM2)
            ;
                (H1, E1, S1) = (H0, E0, S0),
                (LR1, LM1)   = (LR0, LM0)
            )
        ),
        S1 \== nullsit
    ).


final(RewardF, H, E, S) :-
    maybe_final(E),
    \+ (next(E, Ps),
        member((C, R), Ps),
        trans_max_h(RewardF, H, (C ; R), S, _E1, S1, _M),
        reward(RewardF, S1) > reward(RewardF, S)).


trans_max_h(RewardF, H, E, S, E1, S1, M) :-
    H < 0, !,
    error(6, trans_max_h(RewardF, H, E, S, E1, S1, M)).
trans_max_h(_RewardF, H, E, S, E1, S1, M) :-
    H =:= 0, !,
    E1 = E,
    S1 = S,
    M is H.
trans_max_h(RewardF, H, E, S, E1, S1, M) :-
    final(RewardF, H, E, S), !,
    %maybe_final(E), !,
    E1 = E,
    S1 = S,
    M is 2*H*H.
trans_max_h(RewardF, H, E, S, E2, S2, M) :-
    trans(RewardF, H, E, S, H1, E1, S1), !,
    trans_max_h(RewardF, H1, E1, S1, E2, S2, M0),
    M is M0 + 1.
trans_max_h(_RewardF, _H, E, S, E1, S1, M) :-
    E1 = E,
    S1 = S,
    M is 0.



do(RewardF, E, S, S2) :-
    horizon(H),
    ( final(RewardF, H, E, S) ->
        S = S2
    ;
        trans(RewardF, H, E, S, _H1, E1, S1),
        do(RewardF, E1, S1, S2)
    ).


reward(RewardF, S, R) :- holds(R is RewardF, S).


:- export pi/3.
pi([], E0, E1) :-
    !, E0 = E1.
pi([N|Ns], E0, E2) :-
    !, pi(N, E0, E1), pi(Ns, E1, E2).
pi(N :: Domain, E0, E1) :-
    !, is_list(Domain),
    ( param(N, E0), foreach(Val, Domain), fromto(nil, EA, EB, E1) do
        sub(N, Val, E0, EB0),
        ( EA == nil -> EB = EB0 ; EB = (EA # EB0) )
    ).
pi(N :: Domain, E0, E1) :-
    !, domain(Var, Domain), sub(N, Var, E0, E1).
pi(N, E0, E1) :-
    !, sub(N, _, E0, E1).


sub(X1, X2, T1, T2) :-
    var(X1), !, error(2, sub(X1, X2, T1, T2)).
sub(_, _, T1, T2) :-
    var(T1),
    T2 = T1.
sub(X1, X2, T1, T2) :-
    nonvar(T1),
    atom(X1),
    T1 = X1,
    T2 = X2.
sub(X1, X2, T1, T2) :-
    nonvar(T1),
    atom(X1),
    T1 \= X1,
    T1 =..[F|L1],
    sub_list(X1, X2, L1, L2),
    T2 =..[F|L2].
sub([X1|Xs1], [X2|Xs2], T1, T3) :-
    sub(X1, X2, T1, T2),
    sub(Xs1, Xs2, T2, T3).
sub((X1a, X1b), (X2a, X2b), T1, T3) :-
    sub(X1a, X2a, T1, T2),
    sub(X1b, X2b, T2, T3).


sub_list(_, _, [], []).
sub_list(X1, X2, [T1|L1], [T2|L2]) :-
    sub(X1, X2, T1, T2),
    sub_list(X1, X2, L1, L2).


holds([], _) :- !.
holds([P|Q], S) :- !, holds(P, S), holds(Q, S).
holds(P & Q, S) :- !, holds(P, S), holds(Q, S).
holds(P v Q, S) :- !, holds(P, S) ; holds(Q, S).
holds(P => Q, S) :- !, holds(-P v Q, S).
holds(P <=> Q, S) :- !, holds((P => Q) & (Q => P), S).
holds(-(-P), S) :- !, holds(P, S).
holds(-(P & Q), S) :- !, holds(-P v -Q, S).
holds(-(P v Q), S) :- !, holds(-P & -Q, S).
holds(-(P => Q), S) :- !, holds(-(-P v Q), S).
holds(-(P <=> Q), S) :- !, holds(-((P => Q) & (Q => P)), S).
holds(-all(V, P), S) :- !, holds(some(V, -P), S).
holds(-some(V, P), S) :- !, \+ holds(some(V, P), S).
holds(-P, S) :- !, \+ holds(P, S).
holds(all(V, P), S) :- !, holds(-some(V, -P), S).
holds(some(V, P), S) :- !, sub(V, _, P, P1), holds(P1, S).
holds(poss(A), S) :- !, poss(A, S).
holds(X is F, S) :- !, F =..L, append(L, [X], L1), F1 =..L1, holds(F1, S).
holds(F, S) :- \+ \+ fluent(F), !, fluent(W, F), apply(F, [S])@W.
holds(P, _) :- \+ \+ nonfluent(P), !, nonfluent(W, P), call(P)@W.
holds(P, _) :- \+ \+ macro(P, _), !, macro(P, P0), call(P0)@W.
holds(P, _) :- call(P).


ssa(F, A, S) :- gamma_plus(F, A, S).
ssa(F, A, S) :- fluent(W, F), apply(F, [S])@W, \+ gamma_minus(F, A, S).


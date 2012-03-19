% vim:filetype=prolog:textwidth=80:shiftwidth=4:softtabstop=4:expandtab
%
% This is a prGolog interpreter written in ECLiPSe-CLP.
% It's used to benchmark the prGolog implementation in Mercury.
%
% The original ECLiPSe-CLP prGolog interpreter also supports constraints.
% These features were removed to be comparable to the Mercury version.
% (At this point, the Mercury version had no support for constraints.)
% See my diploma thesis or so for the original code.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)

:- module(prgolog).

%:- use_module(solver).
:- use_module(worlds).
:- use_module(library(apply)).

:- export(next/2).
:- export(next2/2).
:- export(trans_atom/4).
:- export(trans/4).
:- export(reward/2).
:- export(do/3).

%:- export(start/2).
%:- export(time/2).
%:- export(time/3).
%:- export(val/4).
:- export(holds/2).
%:- export(holds/3).
%:- export(domain/2).
:- export(ssa/3).

:- export op(700, xfy, in).  /* Interval */
:- export op(800, xfy, &).   /* Conjunction */
:- export op(850, xfy, v).   /* Disjunction */
:- export op(870, xfy, =>).  /* Implication */
:- export op(880, xfy, <=>). /* Equivalence */
:- export op(1100, xfy, ;).   /* Action sequence */
:- export op(1150, xfy, #).   /* Nondeterministic action choice */
:- export op(1175, xfy, \\).  /* Interleaved execution */


test(G, V) :-
    \+ \+ (
        call(G),
        repeat,
        test_and_setval(test_lock, 0, 1),
        setval(test_result, V)
    ),
    getval(test_result, V),
    setval(test_lock, 0).

:- setval(test_lock, 0).

next(P, Ds) :- var(P), !, error(2, next(P, Ds)).
next(nil, []) :- !.
next(A, Ds) :- \+ \+ primitive_action(A), !, Ds = [(A, nil)].
next(B, Ds) :- \+ \+ stochastic_action(B), !, Ds = [(B, nil)].
next(T, Ds) :- \+ \+ T = ?(_), !, Ds = [(T, nil)].
next(P, Ds) :- \+ \+ P = atomic(_), !, Ds = [(P, nil)].
next(P, Ds) :- proc(P, E0), !, next(E0, Ds).
next(pi(V, P), Ds) :- !, pi(V, P, E1), next(E1, Ds).
next(EA # EB, Ds) :- !, next(EA, Ds0), next(EB, Ds1), append(Ds0, Ds1, Ds).
next(star(P), Ds) :-
    !, next(P, Ds0),
    ( param(P), foreach((C, R1), Ds0), foreach((C, R), Ds) do
        R = (R1 ; star(P))
    ).
next(EA ; EB, Ds) :-
    !, next(EA, Ds0),
    ( param(EB), foreach((C, R1), Ds0), foreach((C, R), Ds1) do
        R = (R1 ; EB)
    ),
    ( final(EA) -> next(EB, Ds2) ; Ds2 = [] ),
    append(Ds1, Ds2, Ds).
next(EA \\ EB, Ds) :-
    !, next(EA, Ds0),
    ( param(EB), foreach((C, R1), Ds0), foreach((C, R), Ds1) do
        R = (R1 \\ EB)
    ),
    next(EB, Ds2),
    ( param(EA), foreach((C, R2), Ds2), foreach((C, R), Ds3) do
        R = (EA \\ R2)
    ),
    append(Ds1, Ds3, Ds).
next(P, Ds) :- error(2, next(P, Ds)).

next2(P, Ds) :-
    next(P, DsComplex),
    ( foreach((C, R), DsComplex), fromto([], Ds0, Ds1, Ds) do
        ( C = atomic(Complex) ->
            next2(Complex ; R, DsSub)
        ;
            DsSub = [(C, R)]
        ),
        append(DsSub, Ds0, Ds1)
    ).


final(nil).
final(A) :- \+ \+ primitive_action(A), !, fail.
final(B) :- \+ \+ stochastic_action(B), !, fail.
final(T) :- \+ \+ T = ?(_), !, fail.
final(P) :- \+ \+ P = atomic(_), !, fail.
final(pi(V, P)) :- pi(V, P, P1), final(P1).
final(PA # PB) :- once (final(PA) ; final(PB)).
final(star(_)).
final(PA ; PB) :- final(PA), final(PB).
final(PA \\ PB) :- final(PA), final(PB).


trans_atom(A, S, S1, A1) :-
    \+ \+ primitive_action(A), !,
    AT = A,
    %time(AT, A, T),
    %mintime(TMin),
    %maxtime(TMax),
    %domain(T, TMin..TMax),
    %start(S, T0),
    %call(T0 $=< T),
    poss(AT, S),
    S1 = do(AT, S),
    A1 = A.
trans_atom(B, S, S1, A1) :-
    \+ \+ stochastic_action(B), !,
    random_outcome(B, S, A),
    trans_atom(A, S, S1, A1).
trans_atom(C, S, S1, C1) :-
    C = ?(G), !,
    holds(G, S),
    S1 = S,
    C1 = C.


reward(S, V) :-
    reward_function(RewardF),
    holds(V is RewardF, S).


value(P, S, C, V) :-
    lookahead(L),
    V is value(L, P, S, C).


value(L, P, S, C1, V) :-
    L < 0, !,
    error(6, value(L, P, S, C1, V)).
value(L, _, S, 'invalid action', V) :-
    L =:= 0, !,
    V is reward(S).
value(L, P, S, C, V) :-
    next2(P, Ds),
    ( param(L, S),
      foreach((C, R), Ds),
      fromto('invalid action', C0, C2, C),
      fromto(-1, V0, V2, V2) do
        ( test((    trans_atom(C, S, S1, C1),
                    L1 is new_lookahead(L, C1),
                    V1 is value(L1, R, S1, _),
                    V1 >= V0
                ), (V1, C1))
        ->
            (V2, C2) = (V1, C1)
        ;
            (V2, C2) = (V0, C0)
        )
    ),
    ( ( \+ final(P) ; V2 > reward(S) ) ->
        V is V2
    ;
        V is reward(S)
    ).


trans(P, S, P1, S1) :-
    next2(P, Ds),
    ( Ds = [(C, P1)] ->
        trans_atom(C, S, S1, _)
    ;
        ( param(S),
          foreach((C, R), Ds),
          fromto('invalid action', C0, C2, C),
          fromto(nil, R0, R1, P1),
          fromto(-1, V0, V2, _) do
            V1 is value(C ; R, S, C1),
            ( V1 > V0 ->
                (C2, R1, V2) = (C1, R, V1)
            ;
                (C2, R1, V2) = (C0, R0, V0)
            )
        ),
        C \== 'invalid action',
        trans_atom(C, S, S1, _)
    ).


final(P, S) :-
    final(P),
    reward(S) >= value(P, S, _).


do(P, S, S1) :-
    ( final(P, S) ->
        S = S1
    ;
        trans(P, S, P0, S0),
        do(P0, S0, S1)
    ).


/*
time(AT, A, T) :-
    nonvar(AT),
    functor(AT, F, N1),
    N0 is N1 - 1,
    functor(A, F, N0),
    ( param(AT, A), for(I, 1, N0) do
        arg(I, AT, Arg),
        arg(I, A, Arg)
    ),
    arg(N1, AT, T),
    !.
time(AT, A, T) :- nonvar(A), A=..LA, append(LA, [T], LAT), AT=..LAT, !.
time(AT, A, T) :- error(1, time(AT, A, T)).


time(AT, T) :- functor(AT, _, Arity), arg(Arity, AT, T), !.
time(AT, T) :- error(1, time(AT, T)).


start(S, T) :- var(S), !, error(4, start(S, T)).
start(s0, T) :- !, mintime(T).
start(do(A, _), T) :- !, time(A, T).
start(S, T) :- error(2, start(S, T)).
*/


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
%pi(N :: Domain, E0, E1) :-
%    !, domain(Var, Domain), sub(N, Var, E0, E1).
pi(N, E0, E1) :-
    !, sub(N, _, E0, E1).


/*
domain(Var, Lo..Hi) :- atomic(Lo), atomic(Hi), !, Var :: Lo..Hi.
domain(Var, Lo..Hi) :- !, call(Var $>= Lo), call(Var $=< Hi).
domain(Var, Domain) :- Var :: Domain.
*/


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
holds(X is F, S) :- !, F =..L, append(L, [X], L1), F1 =..L1, holds(F1, S).
%holds(eval(P), S) :- !, start(S, T), holds(eval(P, T), S).
%holds(eval(P, T), S) :- !, holds(P, S, T).
holds(poss(A), S) :- !, poss(A, S).
%holds(start(T), S) :- !, start(S, T).
holds(F, S) :- \+ \+ fluent(F), !, fluent(W, F), apply(F, [S])@W.
holds(P, _) :- \+ \+ nonfluent(P), !, nonfluent(W, P), call(P)@W.
holds(P, _) :- \+ \+ macro(P, _), !, macro(P, P0), call(P0)@W.
holds(P, _) :- call(P).


/*
holds(P, S, T) :- tform(P, S, T, C), call(C).


tform([], _, _, true) :- !.
tform([P|Q], S, T, C0) :- !, tform(P & Q, S, T, C0).
tform(P & Q, S, T, C0 and C1) :- !, tform(P, S, T, C0), tform(Q, S, T, C1).
tform(P v Q, S, T, C0 or C1) :- !, tform(P, S, T, C0), tform(Q, S, T, C1).
tform(-P, S, T, neg(C)) :- !, tform(P, S, T, C).
tform(L < R, S, T, L0 $< R0) :- !, val(L, S, T, L0), val(R, S, T, R0).
tform(L > R, S, T, L0 $> R0) :- !, val(L, S, T, L0), val(R, S, T, R0).
tform(L =< R, S, T, L0 $=< R0) :- !, val(L, S, T, L0), val(R, S, T, R0).
tform(L >= R, S, T, L0 $>= R0) :- !, val(L, S, T, L0), val(R, S, T, R0).
tform(L = R, S, T, L0 $= R0) :- !, val(L, S, T, L0), val(R, S, T, R0).
tform(L \= R, S, T, L0 $\= R0) :- !, val(L, S, T, L0), val(R, S, T, R0).
tform(min(E), S, T, min(E0)) :- !, val(E, S, T, E0).
tform(max(E), S, T, max(E0)) :- !, val(E, S, T, E0).
tform(V in (Lo, Hi), S, T, (Lo $< V0) and (V0 $< Hi)) :- !, val(V, S, T, V0).
tform(V in [Lo, Hi], S, T, (Lo $=< V0) and (V0 $=< Hi)) :- !, val(V, S, T, V0).
tform(L is R, S, T, L0 issi R0) :- !, val(L, S, T, L0), val(R, S, T, R0).
tform(P, S, T, C) :- \+ \+ macro(P, _), !, macro(P, P0), tform(P0, S, T, C).
tform(P, S, T, C) :- error(2, tform(P, S, T, C)).


val(V0, _, _, V1) :- var(V0), !, V1 = V0.
val(V0, _, _, V1) :- number(V0), !, V1 = V0.
val(F1 + F2, S, T, V1 + V2) :- !, val(F1, S, T, V1), val(F2, S, T, V2).
val(F1 - F2, S, T, V1 - V2) :- !, val(F1, S, T, V1), val(F2, S, T, V2).
val(F1 * F2, S, T, V1 * V2) :- !, val(F1, S, T, V1), val(F2, S, T, V2).
val(F1 / F2, S, T, V1 / V2) :- !, val(F1, S, T, V1), val(F2, S, T, V2).
val(F1 ^ F2, S, T, V1 ^ V2) :- !, val(F1, S, T, V1), val(F2, S, T, V2).
val(sin(F), S, T, sin(V)) :- !, val(F, S, T, V).
val(cos(F), S, T, cos(V)) :- !, val(F, S, T, V).
val(tan(F), S, T, tan(V)) :- !, val(F, S, T, V).
val(abs(F), S, T, abs(V)) :- !, val(F, S, T, V).
val(constant(A0), S, T, Term) :-
    !, val(A0, S, T, B0),
    Term = B0.
val(linear(A0, A1, T0), S, T, Term) :-
    !, val(A0, S, T, B0), val(A1, S, T, B1),
    Term = B0 + B1*(T-T0).
val(quadratic(A0, A1, A2, T0), S, T, Term) :-
    !, val(A0, S, T, B0), val(A1, S, T, B1), val(A2, S, T, B2),
    Term = B0 + B1*(T-T0) + B2*(T-T0)*(T-T0).
val(start, S, _, Term) :-
    !, start(S, Term).
val(f_N(Mu, Sigma), S, T, Term) :-
    !, val(Mu, S, T, Mu0), val(Sigma, S, T, Sigma0),
    Term = f_N(Mu0, Sigma0).
val(F, S, T, V) :-
    \+ \+ ccfluent(F), !,
    ccfluent(W, F),
    apply(F, [Func, S])@W,
    val(Func, S, T, V).
val(F, S, T, V) :- error(2, val(F, S, T, V)).
*/


ssa(F, A, S) :- gamma_plus(F, A, S).
ssa(F, A, S) :- fluent(W, F), apply(F, [S])@W, \+ gamma_minus(F, A, S).


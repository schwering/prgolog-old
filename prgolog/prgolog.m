%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2011-2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: prgolog.m.
% Main author: schwering.
%
% A simple Golog interpreter with a decision theory and transition semantics
% based on program program decomposition.
%
% The basic ingredient to write and execute a Golog program, one needs to
% define a basic action theory, short bat.  A bat consists of a type for
% primitive actions, a precondition predicate poss/2, a function reward/1,
% and a functions lookahead/1.
%
% Programs and Execution:
%
% At the lowest level, programs are constituted of primitive actions and test
% actions which.  The latter are effect-less actions which are executable iff
% the test condition holds.  Primitive and test actions can be arranged by
% sequence operators, nondeterminstic branching, nondeterministic looping, and
% concurrency by interleaving.  Furthermore, (sub-) programs in a concurrent
% program can be marked as complex action which prevents interleaving.
%
% Traditional constructs like if-then-else and while-loops can be created with
% the prgolog.nice module.  It also provides a pick operator which basically
% expands to a nondeterministic branch, one for each element in the domain
% from which the value is picked.
%
% Execution of a program builds up a situation term which is basically a
% history of executed actions.
%
% When the interpreter is asked to execute the next action of such a program
% with the trans/4 predicate, it decomposes the program in all possible ways,
% computes the reward/1 after lookahead/2 many steps and then opts for the
% decomposition that promises the highest reward/1.
%
% A program is final/2 if execution may stop, for example because the remaining
% program is a nondeterministic loop, and if further execution does not improve
% the reward.
%
% Stochastic Actions:
%
% Nondeterminism in programs like the aforementioned branch and loop and also
% concurrency represent choice points where the agent might decide what to do
% and in fact does by opting for the reward-maxizing alternative.  Another kind
% of nondeterminism arises when the outcome of actions is not for sure.  Such
% actions are called stochastic.  When a stochastic action is executed, nature
% picks a primitive outcome action at random which is then executed
% deterministcally.
%
% Sampling of these stochastic actions can be easily implemented as folows.
% Each stochastic function is implemented as unary higher-order function whose
% single argument is a situation term and which returns a primitive action.
% Right before execution, the interpreter evaluates this function for the
% current situation and executes the returned primitive action.
%
% Procedure Calls:
%
% Calls of procedures are represented as nullary higher-order functions which
% return the body of the procedure.  The program decomposition evaluates these
% functions and thus replaces the procedure call with the procedure body when
% needed, that is, lazily.
%
% Fluent Formulas:
%
% Each fluent formula is represented as a boolean function that returns `yes'
% if the predicate holds in the given situation and `no' otherwise.  We don't
% use higher-order predicate terms due to a technicality in Mercury: the inst
% of a higher-order predicate term is determined by its mode, not by its type,
% while higher-order functions have a default inst (see Section 8.3
% (``Higher-order modes'') in the Mercury LRM for details).  If we didn't go
% with boolean functions but higher-order predicates instead, we would have to
% add inst definitions for all types just to tell Mercury that relational
% fluents have the boring inst `pred(in) is semidet'.  Further complication
% arises when we use higher-order predicates like solutions and foldl which we
% need to wrap into anonymous lambda expressions to get the modes right.  Some
% general helper predicates and functions to construct fluents (and abstract
% from the aforementioned technicality) are defined in the submodule
% prgolog.fluent.m.
%
%-----------------------------------------------------------------------------%

:- module prgolog.

:- interface.

:- use_module bool.

%-----------------------------------------------------------------------------%

:- type sit(A) ---> s0 ; do(A, sit(A)).

:- type reward == float.
:- type lookahead == int.
:- type value.

:- type funfluent(A, R) == (func(sit(A)) = R).
:- type relfluent(A) == funfluent(A, bool.bool).

:- type proc(A) == ((func) = prog(A)).
:- type primf(A) == (func(sit(A)) = A).

:- type picksucc(A, T) == (func(T, value) = T).
:- type pickprog(A, T) == (func(T) = prog(A)).

:- type atom(A)
    --->    prim(A)
    ;       primf(primf(A))
    ;       test(relfluent(A)).

:- type pseudo_atom(A)
    --->    atom(atom(A))
    ;       complex(prog(A)).

:- type prog(A)
    --->    seq(prog(A), prog(A))
    ;       non_det(prog(A), prog(A))
    ;       conc(prog(A), prog(A))
    ;       star(prog(A))
    ;       some [T] pick(picksucc(A, T), T, pickprog(A, T))
    ;       proc(proc(A))
    ;       pseudo_atom(pseudo_atom(A))
    ;       nil.

%-----------------------------------------------------------------------------%

:- typeclass bat(A) where [
    pred poss(A, sit(A)),
    mode poss(in, in) is semidet,

    func reward(sit(A)) = reward,
    mode reward(in) = out is det,

    func lookahead(sit(A)) = lookahead,
    mode lookahead(in) = out is det
].

%-----------------------------------------------------------------------------%

:- pred trans(prog(A), sit(A), prog(A), sit(A)) <= bat(A).
:- mode trans(in, in, out, out) is semidet.

:- pred final(prog(A), sit(A)) <= bat(A).
:- mode final(in, in) is semidet.

:- pred do(prog(A), sit(A), sit(A)) <= bat(A).
:- mode do(in, in, out) is semidet.

%-----------------------------------------------------------------------------%

:- pred value > value.
:- mode in > in is semidet.

%-----------------------------------------------------------------------------%

:- include_module ccfluent.
:- include_module fluent.
:- include_module nice.
:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- use_module lazy.
:- use_module tree.

%-----------------------------------------------------------------------------%

:- type value == {reward, lookahead}.

:- type pseudo_decomp(A) ---> pseudo_decomp(pseudo_atom(A), prog(A)).
:- type decomp(A) ---> decomp(atom(A), prog(A)).

:- type pickspec(A) ---> some [T] pickspec(picksucc(A, T), T, pickprog(A, T)).
:- type pickbest(A) == (func(pickspec(A)) = prog(A)).

:- type tree(A, T) == tree.tree(T, pickbest(A)).

%-----------------------------------------------------------------------------%

:- func next(prog(A)) = tree(A, pseudo_decomp(A)) <= bat(A).
:- mode next(in) = out is det.

next(seq(P1, P2)) =
    tree.branch(
        ( if final(P1) then next(P2) else tree.empty ),
        tree.map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, seq(R, P2)),
                 next(P1))).
next(non_det(P1, P2)) =
    tree.branch(next(P1), next(P2)).
next(conc(P1, P2)) =
    tree.branch(
        tree.map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, conc(P1, R)),
                 next(P2)),
        tree.map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, conc(R, P2)),
                 next(P1))).
next(pick(F, X0, P)) =
    tree.lazy(func(PickBest) = next(PickBest('new pickspec'(F, X0, P)))).
next(star(P)) =
    tree.map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, seq(R, star(P))),
             next(P)).
next(proc(N)) =
    next(apply(N)).
next(pseudo_atom(C)) =
    tree.value(pseudo_decomp(C, nil)).
next(nil) =
    tree.empty.

%-----------------------------------------------------------------------------%

:- pred final(prog(A)).
:- mode final(in) is semidet.

final(seq(P1, P2)) :-
    final(P1),
    final(P2).
final(non_det(P1, P2)) :-
    (   final(P1)
    ;   final(P2) ).
final(conc(P1, P2)) :-
    final(P1),
    final(P2).
final(pick(_, X0, P)) :-
    final(P(X0)).
final(star(_)).
final(proc(N)) :-
    final(apply(N)).
final(pseudo_atom(atom(_))) :-
    false.
final(pseudo_atom(complex(P))) :-
    final(P).
final(nil).

%-----------------------------------------------------------------------------%

:- func next2(prog(A)) = tree(A, decomp(A)) <= bat(A).
:- mode next2(in) = out is det.

next2(P) =
    tree.mapt((func(pseudo_decomp(C, R)) = T :-
        (   C = complex(P1),
            T = next2(seq(P1, R))
        ;   C = atom(C1),
            T = tree.value(decomp(C1, R))
        )
    ), next(P)).

%-----------------------------------------------------------------------------%

:- pred trans_atom(atom(A), sit(A), sit(A)) <= bat(A).
:- mode trans_atom(in, in, out) is semidet.

trans_atom(prim(A), S, S1) :-
    poss(A, S),
    S1 = do(A, S).
trans_atom(primf(B), S, S1) :-
    A = B(S),
    trans_atom(prim(A), S, S1).
trans_atom(test(T), S, S) :-
    T(S) = bool.yes.

%-----------------------------------------------------------------------------%

{V1, N1} > {V2, N2} :- V1 > V2 ; V1 = V2, N1 > N2.


:- func max(value, value) = value.

max(VN1, VN2) = ( if VN1 > VN2 then VN1 else VN2 ).


:- func value(prog(A), sit(A)) = value <= bat(A).
:- mode value(in, in) = out is det.

value(P, S) = value(P, S, lookahead(S)).


:- func value(prog(A), sit(A), lookahead) = value <= bat(A).
:- mode value(in, in, in) = out is det.

value(P, S, L) = {V, N} :-
    if      L > 0,
            {V2, N2} = tree.foldl((func(decomp(C, R), VN2) = VN3 is det :-
                if      trans_atom(C, S, S1)
                then    {V1, N1} = value(R, S1, L - 1),
                        VN3 = max({V1, N1 + 1}, VN2)
                else    VN3 = VN2
            ), tree.force(pickbest(S), next2(P)), {min, min_int}),
            {min, min_int} \= {V2, N2},
            ( final(P) => V2 > reward(S) )
    then    V = V2, N = N2
    else    V = reward(S), N = ( if final(P) then L else 0 ).

%-----------------------------------------------------------------------------%

:- func fixpoint(func(T) = T, T) = T.

fixpoint(F, X0) = ( if X = X0 then X else fixpoint(F, X) ) :- X = F(X0).


:- func pickbest(sit(A), pickspec(A)) = prog(A) <= bat(A).

pickbest(S, pickspec(F, X0, P)) =
    P(fixpoint(func(X) = F(X, value(P(X), S)), X0)).

%-----------------------------------------------------------------------------%

trans(P, S, P1, S1) :-
    T = tree.force(pickbest(S), next2(P)),
    tree.map_reduce(
        func(D @ decomp(C, R)) =
            {D, lazy.delay((func) = value(seq(pseudo_atom(atom(C)), R), S))},
        func(X1 @ {_, VN1}, X2 @ {_, VN2}) =
            ( if lazy.force(VN1) > lazy.force(VN2) then X1 else X2 ),
    T) = {decomp(C1, P1), _},
    trans_atom(C1, S, S1).

%-----------------------------------------------------------------------------%

final(P, S) :-
    final(P),
    {V, _} = value(P, S),
    reward(S) >= V.

%-----------------------------------------------------------------------------%

do(P, S, S2) :-
    if      final(P, S)
    then    S = S2
    else    trans(P, S, P1, S1),
            do(P1, S1, S2).

%-----------------------------------------------------------------------------%
:- end_module prgolog.
%-----------------------------------------------------------------------------%

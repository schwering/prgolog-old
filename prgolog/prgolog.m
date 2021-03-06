%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2011-2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: prgolog.m.
% Main author: schwering.
%
% A Golog interpreter with a decision theory and transition semantics based on
% program program decomposition.
%
% The basic ingredient to write and execute a Golog program, one needs to
% define a basic action theory, short bat.  A bat consists of a type for
% primitive actions, a precondition predicate poss/2, a function reward/1,
% and a functions lookahead/1.
%
%
% A Note to Prolog Programmers:
% -----------------------------
%
% In Mercury we are subordinate to its (Haskell-ish) type system.  You will see
% that we don't use non-ground terms except variables themselves (Prolog
% interpreters often that, e.g., to implement the pick operator with Reiter's
% sub/4 predicate.)
%
% On the hand, we make extensive use of Mercury's functional programming
% capabilities.
% The simplest example is that we represent procedure calls as higher-order
% functions.
% Another example is that we allow actions to depend on the current situations,
% thus allowing functional fluents.  This turned out to be very useful in many
% applications, e.g., to compute action parameter values at execution time.
% In Prolog-based Golog implementations that is often done in the precondition
% in my experience.  A special case of this is to implement stochastic actions
% via sampling.
% Our pick operator makes extensive use of higher-order functions, too, which
% may not be too easy to grasp, but I think it's still pretty elegant (at least
% I spend very much time on it).
%
%
% General Idea of Programs and Execution:
% ---------------------------------------
%
% Execution of a program builds up a situation term which is basically a
% history of executed actions.
%
% When the interpreter is asked to execute the next action of such a program
% with the trans/4 predicate, it decomposes the program in all possible ways
% -- there may be multiple potential ways to execute the program due to
% nondeterminism --, computes the reward/1 after lookahead/1 many steps and then
% opts for the decomposition that promises the highest reward/1.
%
% A program is final/2 if execution may stop, e.g., because the remaining
% program is a nondeterministic loop, and if further execution does not improve
% the reward.
%
%
% Details of Programs and Execution:
% ----------------------------------
%
% At the lowest level, programs are constituted of primitive actions and test
% actions.  Primitive actions may be either constant or situation-dependent,
% which allows to use functional fluents in the actions.  Test actions are
% effect-less actions which are executable iff the test condition holds.
% Actions can be arranged to form sequences, nondeterministic branches,
% nondeterministic loops, concurrency by nondeterministic interleaving, complex
% atomic actions, nondeterministic pick-a-value, and procedure calls.
%
% All nondeterministic constructs are in fact deterministic, because we use
% decision theory to resolve nondeterminism.  I.e., when the interpreter
% encounters a branching operator (or any other nondeterministic operator), it
% examines which branch leads to the higher reward/1 within lookahead/1.
% and chooses that one.
%
% Let's consider the pick operator because its implementation differs from the
% others.
% The program which depends on the picked variable is represented as unary
% function of the variable's domain.  The interpreter requires that for
% different values the returned program always has the same structure and only
% differs in the occuring actions (ususally their parameters).  If this
% restriction is violated, final/1 may misbehave causing unintended program
% execution.
% Since the interpreter doesn't know the domain of the variable of a pick
% operator (it's an existential type), it cannot do this optimization task
% alone.  For that reason the pick operator requires a user-supplied function
% that searches for the optimal picked value.  However, the interpreter provides
% this search function with an evaluation function and a comparison function, so
% that any generic search procedure is easily applicable.
%
% The idea of complex atomic actions is that no concurrently running program may
% interfere with their execution.  I.e., no actions from another program can
% bet in-between the actions of a complex atomic program.  That's why I call
% them atomic.
%
% Traditional constructs like if-then-else and while-loops can be created with
% the prgolog.nice module.  They are simple macro-expansions using tests and
% nondeterministic branches and/or loops.
%
%
% Stochastic Actions:
% -------------------
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
%
% Procedure Calls:
% ----------------
%
% Calls of procedures are represented as nullary higher-order functions which
% return the body of the procedure.  The program decomposition evaluates these
% functions and thus replaces the procedure call with the procedure body when
% needed, i.e., lazily.
%
%
% Fluent Formulas:
% ----------------
%
% Each fluent formula is represented as a boolean function that returns `yes'
% if the predicate holds in the given situation and `no' otherwise.
% We don't use semidet predicates, because then we would have to carry around
% the program inst's everywhere.  While this might be tolerable, it doesn't work
% with higher-order predicates and functions like foldr.
% Some general helper predicates and functions to construct fluents are defined
% in the submodule prgolog.fluent.m.
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

:- type value_func(U) == (func(U) = value).
:- type maxi_func(U) == (func(U, value_func(U), comparison_func(value)) = U).
:- type pickprog(A, U) == (func(U) = prog(A)).

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
    ;       some [U] pick(maxi_func(U), U, pickprog(A, U))
    ;       proc(proc(A))
    ;       pseudo_atom(pseudo_atom(A))
    ;       nil.

%-----------------------------------------------------------------------------%

:- typeclass bat(A) where [
    pred poss(A, sit(A)),
    mode poss(in, in) is semidet,

    func reward_bound(atom(A)) = reward,
    mode reward_bound(in) = out is det,

    func reward(A, sit(A)) = reward,
    mode reward(in, in) = out is det,

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

:- include_module ccfluent.
:- include_module debug.
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

%-----------------------------------------------------------------------------%

:- func next(prog(A)) = tree.tree(pseudo_decomp(A), value) <= bat(A).
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
        tree.map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, conc(R, P2)),
                 next(P1)),
        tree.map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, conc(P1, R)),
                 next(P2))).
next(pick(G, X0, P)) =
    tree.'new sprout'(G, X0, func(X) = next(P(X))).
next(star(P)) =
    tree.map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, seq(R, star(P))),
             next(P)).
next(proc(N)) =
    next(apply(N)).
next(pseudo_atom(C)) =
    tree.leaf(pseudo_decomp(C, nil)).
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

:- func next2(prog(A)) = tree.tree(decomp(A), value) <= bat(A).
:- mode next2(in) = out is det.

next2(P) =
    tree.mapt((func(pseudo_decomp(C, R)) = T :-
        (   C = complex(P1),
            T = next2(seq(P1, R))
        ;   C = atom(C1),
            T = tree.leaf(decomp(C1, R))
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

:- func min_value = value.

min_value = {-1.0 * float.max, int.min_int}.


:- pred value > value.
:- mode in > in is semidet.

{V1, N1} > {V2, N2} :- V1 > V2 ; V1 = V2, N1 > N2.


:- func cmp(value, value) = comparison_result.

cmp(V, W) = ( if V > W then (>) else if V = W then (=) else (<) ).


:- func max(value, value) = value.

max(VN1, VN2) = ( if VN1 > VN2 then VN1 else VN2 ).


:- func pickbest(sit(A)) = tree.force_args(decomp(A), value) <= bat(A).

pickbest(S) = tree.force_args(Val, cmp, min_value) :-
    Val = (func(decomp(C, R)) = value(seq(pseudo_atom(atom(C)), R), S)).

%-----------------------------------------------------------------------------%

:- func value(prog(A), sit(A)) = value <= bat(A).

value(P, S) = value(P, S, lookahead(S)).


:- func heuristic(lookahead, decomp(A)) = value <= bat(A).

heuristic(L, decomp(C, R)) = {V, L} :-
    if      L > 0,
            {V1, _} = tree.reduce(max, tree.map(heuristic(L - 1), next2(R)))
    then    V = reward_bound(C) + V1
    else    V = reward_bound(C).


:- func value(prog(A), sit(A), lookahead) = value <= bat(A).

value(P, S, L) = {V, N} :-
    if      L > 0,
            tree.max_search(
                cmp, cmp,
                heuristic(L),
                (func(decomp(C, R)) = {V0 + V1, N1 + 1} is semidet :-
                    trans_atom(C, S, S1),
                    V0 = ( if S1 = do(A, S) then reward(A, S) else 0.0 ),
                    {V1, N1} = value(R, S1, L - 1)
                ),
                pickbest(S), next2(P), {V2, N2}, _
            ),
            ( final(P) => V2 > 0.0 )
    then    V = V2, N = N2
    else    V = 0.0, N = ( if final(P) then L else 0 ).

%-----------------------------------------------------------------------------%

trans(P, S, P1, S1) :-
    L = lookahead(S),
    tree.max_search(
        cmp, cmp,
        heuristic(L),
        func(decomp(C, R)) = value(seq(pseudo_atom(atom(C)), R), S, L),
        pickbest(S), next2(P), decomp(C1, P1)
    ),
    trans_atom(C1, S, S1).

%-----------------------------------------------------------------------------%

final(P, S) :-
    final(P),
    {V, _} = value(P, S),
    V =< 0.0.

%-----------------------------------------------------------------------------%

do(P, S, S2) :-
    if      final(P, S)
    then    S = S2
    else    trans(P, S, P1, S1),
            do(P1, S1, S2).

%-----------------------------------------------------------------------------%
:- end_module prgolog.
%-----------------------------------------------------------------------------%

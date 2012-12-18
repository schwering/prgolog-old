%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
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
% primitive actions, a precondition predicate poss/2, a function reward/2,
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
% computes the reward/2 after lookahead/2 many steps and then opts for the
% decomposition that promises the highest reward/2.
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

:- type funfluent(A, R) == (func(sit(A)) = R).
:- type relfluent(A) == funfluent(A, bool.bool).

:- type proc(A) == ((func) = prog(A)).
:- type primf(A) == (func(sit(A)) = A).

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
    ;       proc(proc(A))
    ;       pseudo_atom(pseudo_atom(A))
    ;       nil.

%-----------------------------------------------------------------------------%

:- typeclass bat(A) where [
    pred poss(A, sit(A)),
    mode poss(in, in) is semidet,

    func reward(prog(A), sit(A)) = reward,
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
:- include_module fluent.
:- include_module nice.
:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module solutions.

%-----------------------------------------------------------------------------%

:- type pseudo_decomp(A) ---> pseudo_decomp(pseudo_atom(A), prog(A)).
:- type decomp(A) ---> decomp(atom(A), prog(A)).

%-----------------------------------------------------------------------------%

:- func next(prog(A)) = list(pseudo_decomp(A)) <= bat(A).
:- mode next(in) = out is det.
:- mode next(in) = in is semidet.

next(seq(P1, P2)) =
    ( if final(P1) then next(P2) else [] ) ++
    map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, seq(R, P2)), next(P1)).
next(non_det(P1, P2)) =
    next(P1) ++ next(P2).
next(conc(P1, P2)) =
    map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, conc(P1, R)), next(P2)) ++
    map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, conc(R, P2)), next(P1)).
next(star(P)) =
    map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, seq(R, star(P))), next(P)).
next(proc(N)) =
    next(apply(N)).
next(pseudo_atom(C)) =
    [pseudo_decomp(C, nil)].
next(nil) =
    [].

%-----------------------------------------------------------------------------%

:- pred final(prog(A)) <= bat(A).
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
final(star(_)).
final(proc(N)) :-
    final(apply(N)).
final(pseudo_atom(_)) :-
    false.
final(nil).

%-----------------------------------------------------------------------------%

:- func next2(prog(A)) = list(decomp(A)) <= bat(A).
:- mode next2(in) = out is det.
:- mode next2(in) = in is semidet.

next2(P) =
    foldl((func(pseudo_decomp(C, R), Ds1) = Ds ++ Ds1 :-
        (   C = complex(P1),
            Ds = map(func(decomp(C1, R1)) = decomp(C1, seq(R1, R)), next2(P1))
        ;   C = atom(C1),
            Ds = [decomp(C1, R)]
        )
    ), next(P), []).

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

:- pred ({reward, lookahead}::in) > ({reward, lookahead}::in) is semidet.

{V1, N1} > {V2, N2} :- V1 > V2 ; V1 = V2, N1 > N2.


:- func max({reward, lookahead}::in, {reward, lookahead}::in) =
    ({reward, lookahead}::out) is det.

max(VN1, VN2) = ( if VN1 > VN2 then VN1 else VN2 ).


:- func value(prog(A), sit(A), lookahead) = {reward, lookahead} <= bat(A).
:- mode value(in, in, in) = out is det.

value(P, S, L) = {V, N} :-
    if      L > 0,
            {V2, N2} = foldl((func(decomp(C, R), VN2) = VN3 is det :-
                if      trans_atom(C, S, S1)
                then    {V1, N1} = value(R, S1, L - 1),
                        VN3 = max({V1, N1 + 1}, VN2)
                else    VN3 = VN2
            ), next2(P), {min, min_int}),
            {min, min_int} \= {V2, N2},
            ( final(P) => V2 > reward(P, S) )
    then    V = V2, N = N2
    else    V = reward(P, S), N = ( if final(P) then L else 0 ).

%-----------------------------------------------------------------------------%

trans(P, S, P1, S1) :-
    Ds = next2(P),
    (   if      Ds = [D]
        then    decomp(C1, P1) = D
        else    {decomp(C1, P1), _} = foldl(func(D, VN) = Reduce(Map(D), VN),
                    tail(Ds), Map(head(Ds))),
                Map = (func(D @ decomp(C, R)) = {D, VN} is det :-
                    VN = value(seq(pseudo_atom(atom(C)), R), S, lookahead(S))
                ),
                Reduce = (func({D1, VN1}, {D2, VN2}) =
                    ( if VN1 > VN2 then {D1, VN1} else {D2, VN2} )
                )
    ),
    trans_atom(C1, S, S1).

%-----------------------------------------------------------------------------%

final(P, S) :-
    final(P),
    {V, _} = value(P, S, lookahead(S)),
    reward(P, S) >= V.

%-----------------------------------------------------------------------------%

do(P, S, S2) :-
    if      final(P, S)
    then    S = S2
    else    trans(P, S, P1, S1),
            do(P1, S1, S2).

%-----------------------------------------------------------------------------%
:- end_module prgolog.
%-----------------------------------------------------------------------------%

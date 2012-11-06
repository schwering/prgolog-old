%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: prgolog.m.
% Main author: schwering.
%
% A simple Golog interpreter with decomposition semantics, stochastic actions,
% and decision theory.
%
% Conventions:
% 1. A stands for primitive action
% 2. B for stochastic action
% 3. T for test condition or action
% 4. P for procedures.
%
% The user needs to define a type for primitive actions, stochastic actions
% and procedures.  For these types, the type class bat must be implemented,
% which together gives us everything we need of a basic action theory.  Note
% that the lookahead must be positive and reward non-negative.
%
% The core predicates are:
% * next/2 to decompose a program into one next atomic action its remainder,
% * next2/2 to resolve complex atomic actions,
% * trans_atom/3 to execute primitive, stochastic (sampling), and test actions,
% * trans/4 to pick one decomposition and execute its next step,
% * final/2 decides whether or not a program is final,
% * do/3 that executes a program until it's final.
%
% The interpreter features sequence, recursive procedure calls,
% nondeterministic branch, nondeterministic loop, concurrency through
% nondeterministic interleaving, sequence, test actions, primitive actions,
% stochastic actions, and atomic complex actions.  Nondeterminism is resolved
% by choosing the alternative that maximizes a reward after a lookahead
% lookahead defined in the BAT.
%
% To implement fluent formulas in test actions, we exploit Mercury's
% higher-order types.  Each fluent predicate is represented as a boolean
% function that returns `yes' if the predicate holds in the given situation
% and `no' otherwise.  The fact that we don't use higher-order predicate terms
% is due to a technicality in Mercury: the inst of a higher-order predicate
% term is determined by its mode, not by its type, while higher-order
% functions have a default inst (see Section 8.3 (``Higher-order modes'') in
% the Mercury LRM for details).  If we didn't go with boolean functions but
% higher-order predicates instead, we would have to add inst definitions for
% all types just to tell Mercury that relational fluents have the boring inst
% `pred(in) is semidet'.  Further complication arises when we use higher-order
% predicates like solutions and foldl which we need to wrap into anonymous
% lambda expressions to get the modes right.  Some general helper predicates
% and functions to construct fluents (and abstract from the aforementioned
% technicality) are defined in the submodule prgolog.fluent.m.
%
% The pick operator is not implemented.  It seems to be difficult to do so:
% Higher-order terms apparently cannot contain unbound variables, and I don't
% know how to inspect and modify terms.  I guess it is pretty difficult to
% express this in the type system.  Maybe one needs the univ type and/or the
% deconstruct module or so.
%
% The type system makes the Mercury terms that represent Golog terms more
% complex.  For more concise programs, one may use the nice syntax layer that
% resides in the submodule prgolog.nice.
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

:- type atom(A, B)
    --->    prim(A)
    ;       stoch(B)
    ;       test(relfluent(A)).

:- type pseudo_atom(A, B)
    --->    atom(atom(A, B))
    ;       complex(prog(A, B)).

:- type proc(A, B) == ((func) = prog(A, B)).

:- type prog(A, B)
    --->    seq(prog(A, B), prog(A, B))
    ;       non_det(prog(A, B), prog(A, B))
    ;       conc(prog(A, B), prog(A, B))
    ;       star(prog(A, B))
    ;       proc(proc(A, B))
    ;       pseudo_atom(pseudo_atom(A, B))
    ;       nil.

%-----------------------------------------------------------------------------%

:- typeclass bat(A, B) <= (A -> B) where [
    pred poss(A, A, sit(A)),
    mode poss(in, out, in) is semidet,

    pred random_outcome(B, A, sit(A)),
    mode random_outcome(in, out, in) is det,

    func reward(prog(A, B), sit(A)) = reward,
    mode reward(in, in) = out is det,

    func lookahead(sit(A)) = lookahead,
    mode lookahead(in) = out is det,

    func new_lookahead(lookahead, atom(A, B)) = lookahead,
    mode new_lookahead(in, in) = out is det
].

%-----------------------------------------------------------------------------%

:- pred trans(prog(A, B), sit(A), prog(A, B), sit(A)) <= bat(A, B).
:- mode trans(in, in, out, out) is semidet.

:- pred final(prog(A, B), sit(A)) <= bat(A, B).
:- mode final(in, in) is semidet.

:- pred do(prog(A, B), sit(A), sit(A)) <= bat(A, B).
:- mode do(in, in, out) is semidet.

%-----------------------------------------------------------------------------%

:- include_module ccfluent.
:- include_module fluent.
:- include_module nice.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module solutions.

%-----------------------------------------------------------------------------%

:- pred next(prog(A, B), pseudo_atom(A, B), prog(A, B))
    <= bat(A, B).
:- mode next(in, out, out) is nondet.
:- mode next(in, in, in) is semidet.

next(seq(P1, P2), C, R) :-
    (   next(P1, C, R1), R = seq(R1, P2)
    ;   next(P2, C, R), final(P1) ).
next(non_det(P1, P2), C, R) :-
    (   next(P1, C, R)
    ;   next(P2, C, R) ).
next(conc(P1, P2), C, R) :-
    (   next(P1, C, R1), R = conc(R1, P2)
    ;   next(P2, C, R2), R = conc(P1, R2) ).
next(star(P), C, R) :-
    next(P, C, R1),
    R = seq(R1, star(P)).
next(proc(N), C, R) :-
    P = apply(N),
    next(P, C, R).
next(pseudo_atom(C), C, R) :-
    R = nil.
next(nil, _, _) :-
    false.

%-----------------------------------------------------------------------------%

:- pred final(prog(A, B)) <= bat(A, B).
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
final(pseudo_atom(_)) :-
    false.
final(nil).

%-----------------------------------------------------------------------------%

:- pred next2(prog(A, B), atom(A, B), prog(A, B)) <= bat(A, B).
:- mode next2(in, out, out) is nondet.

next2(P, C, R) :-
    next(P, C1, R1),
    (
        C1 = complex(P1),
        next2(P1, C, R0),
        R = seq(R0, R1)
    ;
        C1 = atom(C),
        R = R1
    ).

%-----------------------------------------------------------------------------%

:- pred trans_atom(atom(A, B), sit(A), sit(A)) <= bat(A, B).
:- mode trans_atom(in, in, out) is semidet.

trans_atom(prim(A), S, S1) :-
    poss(A, A1, S),
    S1 = do(A1, S).
trans_atom(stoch(B), S, S1) :-
    random_outcome(B, A, S),
    trans_atom(prim(A), S, S1).
trans_atom(test(T), S, S) :-
    T(S) = bool.yes.

%-----------------------------------------------------------------------------%

:- pred '>'({reward, lookahead}::in, {reward, lookahead}::in) is semidet.

'>'({V1, N1}, {V2, N2}) :- V1 > V2 ; V1 = V2, N1 > N2.


:- func max({reward, lookahead}::in, {reward, lookahead}::in) =
    ({reward, lookahead}::out) is det.

max(VN1, VN2) = ( if VN1 > VN2 then VN1 else VN2 ).


:- func value(prog(A, B), sit(A), lookahead) = {reward, lookahead} <= bat(A, B).
:- mode value(in, in, in) = out is det.

value(P, S, L) = {V, N} :-
    if      L > 0,
            solutions((pred({V1, N1 + 1}::out) is nondet :-
                next2(P, C1, R1),
                trans_atom(C1, S, S1),
                {V1, N1} = value(R1, S1, new_lookahead(L, C1))
            ), Values),
            Values \= [],
            {V2, N2} = list.foldl(max, Values, {min, min_int}),
            ( final(P) => V2 > reward(P, S) )
    then    V = V2, N = N2
    else    V = reward(P, S), N = ( if final(P) then L else 0 ).

%-----------------------------------------------------------------------------%

:- type decomp(A, B) ---> decomp(atom(A, B), prog(A, B)).
:- type cand(A, B) ---> cand(decomp(A, B), value :: {reward, lookahead}).


:- func new_cand(sit(A), decomp(A, B)) = cand(A, B) <= bat(A, B).
:- mode new_cand(in, in) = out is det.

new_cand(S, decomp(C, R)) = cand(decomp(C, R), {V, N}) :-
    {V, N} = value(seq(pseudo_atom(atom(C)), R), S, lookahead(S)).


:- func fold(sit(A), decomp(A, B), cand(A, B)) = cand(A, B) <= bat(A, B).
:- mode fold(in, in, in) = out is det.

fold(S, D, Y) = ( if X = new_cand(S, D), value(X) > value(Y) then X else Y ).


trans(P, S, P1, S1) :-
    solutions((pred(decomp(C, R)::out) is nondet :-
        next2(P, C, R)
    ), Ds),
    (   if      Ds = [D]
        then    D = decomp(C1, P1)
        else    Ds = [D | Ds0],
                cand(decomp(C1, P1), _) =
                    list.foldl(fold(S), Ds0, new_cand(S, D))
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

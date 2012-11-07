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

:- type proc(A) == ((func) = prog(A)).
:- type stoch(A) == (func(sit(A)) = A).

:- type atom(A)
    --->    prim(A)
    ;       stoch(stoch(A))
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
    pred poss(A, A, sit(A)),
    mode poss(in, out, in) is semidet,

    func reward(prog(A), sit(A)) = reward,
    mode reward(in, in) = out is det,

    func lookahead(sit(A)) = lookahead,
    mode lookahead(in) = out is det,

    func new_lookahead(lookahead, atom(A)) = lookahead,
    mode new_lookahead(in, in) = out is det
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- include_module test. :- implementation.

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
    map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, conc(P2, R)), next(P1)) ++
    map(func(pseudo_decomp(C, R)) = pseudo_decomp(C, conc(P1, R)), next(P2)).
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
    poss(A, A1, S),
    S1 = do(A1, S).
trans_atom(stoch(B), S, S1) :-
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
                then    {V1, N1} = value(R, S1, new_lookahead(L, C)),
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
        else    {decomp(C1, P1), _} = foldl(func(D, VN) = Red(Map(D), VN),
                    tail(Ds), Map(head(Ds))
                ),
                Map = (func(D @ decomp(C, R)) = {D, VN} is det :-
                    VN = value(seq(pseudo_atom(atom(C)), R), S, lookahead(S))
                ),
                Red = (func({D1, VN1}, {D2, VN2}) =
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

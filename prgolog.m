% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
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
% and procedures.
% For these types, the type class bat must be implemented, which together
% gives us everything we need of a basic action theory.
% Note that the lookahead must be positive and reward non-negative.
%
% The core predicates are:
% * next/2 to decompose a program into one next atomic action its remainder,
% * next2/2 to resolve complex atomic actions,
% * trans_atom/3 to execute primitive, stochastic (sampling), and test actions,
% * trans/4 to pick one decomposition and execute its next step,
% * final/2 decides whether or not a program is final,
% * do/3 that executes a program until it's final.
%
% The interpreter features sequence, recursive procedure calls, nondeterministic
% branch, nondeterministic loop, concurrency through nondeterministic
% interleaving, sequence, test actions, primitive actions, stochastic actions,
% and atomic complex actions.
% Nondeterminism is resolved by choosing the alternative that maximizes a
% reward after a lookahead lookahead defined in the BAT.
%
% In contrast to typical Prolog implementations, fluent in test actions are not
% interpreted with a holds/2 predicate that deconstructs the expression and
% eventually restores the situation term.
% Instead, a test action takes a unary higher-order predicate whose single
% argument is a situation term.
% Some helper predicates and functions to construct are defined in the submodule
% prgolog.fluents.m.
%
% The pick operator is not implemented. It seems to be difficult to do so:
% Higher-order terms apparently cannot contain unbound variables, and I
% don't know how to inspect and modify terms.
% I guess it is pretty difficult to express this in the type system. Maybe
% one needs the univ type and/or the deconstruct module or so.
%
% The type system makes the Mercury terms that represent Golog terms more
% complex. For more concise programs, one may use the nice syntax layer that
% resides in the submodule prgolog.nice.
%
% Christoph Schwering (schwering@gmail.com)

:- module prgolog.

:- interface.

:- type sit(A) ---> s0 ; do(A, sit(A)).

:- type reward == int.
:- type lookahead == int.

:- type relfluent(A) == pred(sit(A)).
:- inst relfluent == (pred(in) is semidet).
:- type funfluent(A, R) == (func(sit(A)) = R).
:- inst funfluent == (func(in) = out is det).

:- type atom(A, B)
    --->    prim(A)
    ;       stoch(B)
    ;       test(relfluent(A)).
:- inst atom
    --->    prim(ground)
    ;       stoch(ground)
    ;       test(relfluent).

:- type pseudo_atom(A, B, P)
    --->    atom(atom(A, B))
    ;       complex(prog(A, B, P)).
:- inst pseudo_atom
    --->    atom(atom)
    ;       complex(prog).

:- type prog(A, B, P)
    --->    seq(prog(A, B, P), prog(A, B, P))
    ;       non_det(prog(A, B, P), prog(A, B, P))
    ;       conc(prog(A, B, P), prog(A, B, P))
    ;       star(prog(A, B, P))
    ;       proc(P)
    ;       pseudo_atom(pseudo_atom(A, B, P))
    ;       nil.
:- inst prog
    --->    seq(prog, prog)
    ;       non_det(prog, prog)
    ;       conc(prog, prog)
    ;       star(prog)
    ;       proc(ground)
    ;       pseudo_atom(pseudo_atom)
    ;       nil.


:- typeclass bat(A, B, P) <= ((A -> B), (A, B -> P)) where [
    pred poss(A, sit(A)),
    mode poss(in, in) is semidet,

    pred random_outcome(B, A, S),
    mode random_outcome(in, out, in) is det,

    func reward(prog(A, B, P), sit(A)) = reward,
    mode reward(in(prog), in) = out is det,

    func lookahead(sit(A)) = lookahead,
    mode lookahead(in) = out is det,

    func new_lookahead(lookahead, atom(A, B)) = lookahead,
    mode new_lookahead(in, in) = out is det,

    pred proc(P, prog(A, B, P)),
    mode proc(in(ground), out(prog)) is det
].


:- pred trans(prog(A, B, P), sit(A), prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode trans(in(prog), in, out(prog), out) is semidet.

:- pred do(prog(A, B, P), sit(A), sit(A)) <= bat(A, B, P).
:- mode do(in(prog), in, out) is semidet.

:- pred final(prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode final(in(prog), in) is semidet.

:- include_module fluents.
:- include_module nice.


:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.


:- pred next(prog(A, B, P), pseudo_atom(A, B, P), prog(A, B, P)) <= bat(A, B, P).
:- mode next(in(prog), out(pseudo_atom), out(prog)) is nondet.

next(seq(P1, P2), C, R) :-
    (   next(P1, C, R1), R = seq(R1, P2)
    ;   next(P2, C, R), maybe_final(P1) ).
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
    proc(N, P),
    next(P, C, R).
next(pseudo_atom(C), C, R) :-
    R = nil.
next(nil, _, _) :-
    false.


:- pred maybe_final(prog(A, B, P)) <= bat(A, B, P).
:- mode maybe_final(in(prog)) is semidet.

maybe_final(seq(P1, P2)) :-
    maybe_final(P1),
    maybe_final(P2).
maybe_final(non_det(P1, P2)) :-
    (   maybe_final(P1)
    ;   maybe_final(P2) ).
maybe_final(conc(P1, P2)) :-
    maybe_final(P1),
    maybe_final(P2).
maybe_final(star(_)).
maybe_final(pseudo_atom(_)) :-
    false.
maybe_final(nil).


:- pred next2(prog(A, B, P), atom(A, B), prog(A, B, P)) <= bat(A, B, P).
:- mode next2(in(prog), out(atom), out(prog)) is nondet.

next2(P, C, R) :-
    next(P, C1, R1),
    (   C1 = complex(P1),
        next2(P1, C, R0),
        R = seq(R0, R1)
    ;   C1 = atom(C),
        R = R1
    ).


:- pred trans_atom(atom(A, B), sit(A), sit(A)) <= bat(A, B, P).
:- mode trans_atom(in(atom), in, out) is semidet.

trans_atom(prim(A), S, S1) :-
    poss(A, S),
    S1 = do(A, S).
trans_atom(stoch(B), S, S1) :-
    random_outcome(B, A, S),
    trans_atom(prim(A), S, S1).
trans_atom(test(T), S, S) :-
    T(S).


:- func value(prog(A, B, P), sit(A), lookahead) = reward <= bat(A, B, P).
:- mode value(in(prog), in, in) = out is det.

value(P, S, L) = V :-
    if      L > 0,
            solutions((pred(V1::out) is nondet :-
                next2(P, C1, P1),
                trans_atom(C1, S, S1),
                V1 = value(P1, S1, new_lookahead(L, C1)),
                ( maybe_final(P1) => V1 > reward(P, S) )
            ), Values),
            Values \= []
    then    V = list.foldl(int.max, Values, int.min_int)
    else    V = reward(P, S).


:- type decomp(A, B, P) ---> decomp(atom(A, B), prog(A, B, P)).
:- inst decomp ---> decomp(atom, prog).

:- type cand(A, B, P) ---> cand(prog(A, B, P), sit(A), value :: reward).
:- inst cand ---> cand(prog, ground, ground).

trans(P, S, P1, S1) :-
    (pred(DecompsTmp::out(list(decomp))) is det :-
        solutions((pred(decomp(C, R)::out(decomp)) is nondet :-
            next2(P, C, R)
        ), DecompsTmp)
    )(Decomps),
    (   if
            Decomps = [Decomp]
        then
            Decomp = decomp(C1, P1),
            trans_atom(C1, S, S1)
        else
            InitCand = cand(nil, s0, int.min_int),
            list.foldl((pred(decomp(C2, P2)::in(decomp),
                             Cand1::in(cand),
                             Better::out(cand)) is det :-
                if      trans_atom(C2, S, S2),
                        V2 = value(P2, S2, new_lookahead(lookahead(S), C2)),
                        V2 > value(Cand1)
                then    Better = cand(P2, S2, V2)
                else    Better = Cand1
            ), Decomps, InitCand, BestCand),
            BestCand \= InitCand,
            BestCand = cand(P1, S1, _)
    ).


do(P, S, S2) :-
    if      final(P, S)
    then    S = S2
    else    trans(P, S, P1, S1),
            do(P1, S1, S2).


final(P, S) :-
    maybe_final(P),
    reward(P, S) >= value(P, S, lookahead(S)).


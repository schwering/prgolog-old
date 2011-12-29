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

:- module prgolog.

:- interface.

:- type sit(A) ---> s0 ; do(A, sit(A)).

:- type reward == int.

:- type relfluent(A) == pred(sit(A)).
:- type funfluent(A, R) == (func(sit(A)) = R).

:- type test(A)
    --->    and(test(A), test(A))
    ;       or(test(A), test(A))
    ;       neg(test(A))
    ;       rf(relfluent(A)).

:- type atom(A, B)
    --->    prim(A)
    ;       stoch(B)
    ;       test(test(A)).

:- type pseudo_atom(A, B, P)
    --->    atom(atom(A, B))
    ;       complex(prog(A, B, P)).

:- type prog(A, B, P)
    --->    seq(prog(A, B, P), prog(A, B, P))
    ;       non_det(prog(A, B, P), prog(A, B, P))
    ;       conc(prog(A, B, P), prog(A, B, P))
    ;       proc(P)
    ;       pseudo_atom(pseudo_atom(A, B, P))
    ;       nil.

:- typeclass bat(A, B, P) <= ((A -> B), (A, B -> P)) where [
    pred poss(A, sit(A)),
    mode poss(in, in) is semidet,

    pred random_outcome(B, A, S),
    mode random_outcome(in, out, in) is det,

    func reward(sit(A)) = reward,
    mode reward(in) = out is det,

    pred proc(P, prog(A, B, P)),
    mode proc(in, out) is det
].

:- pred holds(test(A), sit(A)).
:- mode holds(in, in) is semidet.

:- pred next(prog(A, B, P), pseudo_atom(A, B, P), prog(A, B, P)) <= bat(A, B, P).
:- mode next(in, out, out) is nondet.

:- pred next2(prog(A, B, P), atom(A, B), prog(A, B, P)) <= bat(A, B, P).
:- mode next2(in, out, out) is nondet.

:- pred trans_atom(atom(A, B), sit(A), sit(A)) <= bat(A, B, P).
:- mode trans_atom(in, in, out) is semidet.

:- pred trans(prog(A, B, P), sit(A), prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode trans(in, in, out, out) is semidet.


:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

holds(T, S) :-
    (   T = and(T1, T2), holds(T1, S), holds(T2, S)
    ;   T = or(T1, T2), ( holds(T1, S) ; holds(T2, S) )
    ;   T = neg(T1), not holds(T1, S)
    ;   T = rf(P)%, P(S)
    ).

next(P, C, R) :-
    (   P = seq(P1, P2),
        next(P1, C, R1),
        R = seq(R1, P2)
    ;   P = non_det(P1, P2),
        (   next(P1, C, R)
        ;   next(P2, C, R)
        )
    ;   P = conc(P1, P2),
        (   next(P1, C, R1),
            R = conc(R1, P2)
        ;   next(P2, C, R2),
            R = conc(P1, R2)
        )
    ;   P = proc(N),
        proc(N, P1),
        next(P1, C, R)
    ;   P = pseudo_atom(C),
        R = nil
    ;   P = nil,
        false
    ).

next2(P, C, R) :-
    next(P, C1, R1),
    (   C1 = complex(P1),
        next2(P1, C, R0),
        R = seq(R0, R1)
    ;   C1 = atom(C),
        R = R1
    ).

trans_atom(C, S, S1) :-
    (   C = prim(A),
        poss(A, S),
        S1 = do(A, S)
    ;   C = stoch(B),
        random_outcome(B, A, S),
        trans_atom(prim(A), S, S1)
    ;   C = test(T),
        holds(T, S),
        S1 = S
    ).

:- type cand(A, B, P) ---> cand(prog  :: prog(A, B, P),
                                sit   :: sit(A),
                                value :: reward).

trans(P, S, P1, S1) :-
    solutions((pred(Cand::out) is nondet :- (
        next2(P, C, P_Cand),
        trans_atom(C, S, S_Cand),
        V_Cand = reward(S_Cand),
        Cand = cand(P_Cand, S_Cand, V_Cand)
    )), Cands),
    Cands = [HeadCand|RestCands],
    list.foldl((pred(NewCand::in, OldCand::in, BetterCand::out) is det :- (
        if      value(NewCand) > value(OldCand)
        then    BetterCand = NewCand
        else    BetterCand = OldCand
    )), RestCands, HeadCand, BestCand),
    P1 = prog(BestCand),
    S1 = sit(BestCand).


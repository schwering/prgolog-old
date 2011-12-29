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
:- type horizon == int.

:- type relfluent(A) == pred(sit(A)).
:- type funfluent(A, R) == (func(sit(A)) = R).

:- type atom(A, B)
    --->    prim(A)
    ;       stoch(B)
    ;       test(relfluent(A)).

:- inst semidet_atom
    --->    prim(ground)
    ;       stoch(ground)
    ;       test(pred(in) is semidet).

:- type pseudo_atom(A, B, P)
    --->    atom(atom(A, B))
    ;       complex(prog(A, B, P)).

:- inst semidet_pseudo_atom
    --->    atom(semidet_atom)
    ;       complex(semidet_prog).

:- type prog(A, B, P)
    --->    seq(prog(A, B, P), prog(A, B, P))
    ;       non_det(prog(A, B, P), prog(A, B, P))
    ;       conc(prog(A, B, P), prog(A, B, P))
    ;       star(prog(A, B, P))
    ;       proc(P)
    ;       pseudo_atom(pseudo_atom(A, B, P))
    ;       nil.

:- inst semidet_prog
    --->    seq(semidet_prog, semidet_prog)
    ;       non_det(semidet_prog, semidet_prog)
    ;       conc(semidet_prog, semidet_prog)
    ;       star(semidet_prog)
    ;       proc(ground)
    ;       pseudo_atom(semidet_pseudo_atom)
    ;       nil.


% Basic action theory.
% This class consists of a type for primitive actions A, stochastic actions B,
% and procedures P.
% For these types, the precondition axioms poss/2 must be defined as in
% ordinary situation calculus.
% Stochastic actions are sampled with random_outcome/3.
% The function reward/2 is used to resolve nonderminism.
% The predicate proc/2 must expand procedures.

:- typeclass bat(A, B, P) <= ((A -> B), (A, B -> P)) where [
    pred poss(A, sit(A)),
    mode poss(in, in) is semidet,

    pred random_outcome(B, A, S),
    mode random_outcome(in, out, in) is det,

    func reward(sit(A)) = reward,
    mode reward(in) = out is det,

    func horizon(sit(A)) = horizon,
    mode horizon(in) = out is det,

    func new_horizon(horizon, atom(A, B)) = horizon,
    mode new_horizon(in, in) = out is det,

    pred proc(P, prog(A, B, P)),
    mode proc(in(ground), out(semidet_prog)) is det
].


% Fluent evaluation.
% In fact, we don't use some predicate holds/2 to interpret fluents, but use
% higher-order terms consisting of the below predicates and actual fluents as
% leaves.

:- pred eq(funfluent(A, R), funfluent(A, R), sit(A)).
:- mode eq(in(func(in) = out is semidet),
           in(func(in) = out is semidet), in) is semidet.

:- pred and(relfluent(A), relfluent(A), sit(A)).
:- mode and(in(pred(in) is semidet), in(pred(in) is semidet), in) is semidet.

:- pred or(relfluent(A), relfluent(A), sit(A)).
:- mode or(in(pred(in) is semidet), in(pred(in) is semidet), in) is semidet.

:- pred neg(relfluent(A), sit(A)).
:- mode neg(in(pred(in) is semidet), in) is semidet.


% Interpreter.
% The interpreter decomposes a program using next/3, maybe_final/1, and next2/3.
% Atomic actions (primitive, stochastic, test) are executed by trans_atom/3.
% Then, trans/4 puts things together.

:- pred next(prog(A, B, P),
             pseudo_atom(A, B, P), prog(A, B, P)) <= bat(A, B, P).
:- mode next(in(semidet_prog),
             out(semidet_pseudo_atom), out(semidet_prog)) is nondet.

:- pred maybe_final(prog(A, B, P)) <= bat(A, B, P).
:- mode maybe_final(in(semidet_prog)) is semidet.

:- pred next2(prog(A, B, P), atom(A, B), prog(A, B, P)) <= bat(A, B, P).
:- mode next2(in(semidet_prog), out(semidet_atom), out(semidet_prog)) is nondet.

:- pred trans_atom(atom(A, B), sit(A), sit(A)) <= bat(A, B, P).
:- mode trans_atom(in(semidet_atom), in, out) is semidet.

:- pred trans(prog(A, B, P), sit(A), prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode trans(in(semidet_prog), in, out(semidet_prog), out) is semidet.


:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

eq(Lhs, Rhs, S) :- Lhs(S) = Rhs(S).
and(T1, T2, S) :- T1(S), T2(S).
or(T1, T2, S) :- T1(S) ; T2(S).
neg(T, S) :- not T(S).

next(P, C, R) :-
    (   P = seq(P1, P2),
        (   next(P1, C, R1),
            R = seq(R1, P2)
        ;   maybe_final(P1),
            next(P2, C, R)
        )
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
    ;   P = star(P1),
        next(P1, C, R1),
        R = seq(R1, star(P1))
    ;   P = proc(N),
        proc(N, P1),
        next(P1, C, R)
    ;   P = pseudo_atom(C),
        R = nil
    ;   P = nil,
        false
    ).

maybe_final(P) :-
    (   P = seq(P1, P2),
        maybe_final(P1),
        maybe_final(P2)
    ;   P = non_det(P1, P2),
        (   maybe_final(P1)
        ;   maybe_final(P2)
        )
    ;   P = conc(P1, P2),
        maybe_final(P1),
        maybe_final(P2)
    ;   P = star(_)
    ;   P = pseudo_atom(_),
        false
    ;   P = nil
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
        T(S),
        S1 = S
    ).

:- type cand(A, B, P) ---> cand(rest_horizon :: horizon,
                                prog         :: prog(A, B, P),
                                sit          :: sit(A),
                                value        :: reward,
                                succ_trans   :: int).
                                

:- pred trans(horizon, prog(A, B, P), sit(A),
              horizon, prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode trans(in, in(semidet_prog), in,
              out, out(semidet_prog), out) is semidet.

trans(H, P, S, H1, P1, S1) :-
    H >= 1,
    solutions((pred(Cand::out) is nondet :- (
        next2(P, C, P_Cand),
        trans_atom(C, S, S_Cand),
        H_Cand = new_horizon(H, C),
        trans_max_h(H_Cand, P_Cand, S_Cand, S_Horizon, SuccTrans),
        V_Cand = reward(S_Horizon),
        Cand = cand(H_Cand, P_Cand, S_Cand, V_Cand, SuccTrans)
    )), Cands),
    Cands = [HeadCand|RestCands],
    list.foldl((pred(NewCand::in, OldCand::in, BetterCand::out) is det :- (
        if      (   value(NewCand) > value(OldCand)
                ;   value(NewCand) = value(OldCand),
                    succ_trans(NewCand) > succ_trans(OldCand)
                )
        then    BetterCand = NewCand
        else    BetterCand = OldCand
    )), RestCands, HeadCand, BestCand),
    H1 = rest_horizon(BestCand),
    P1 = prog(BestCand),
    S1 = sit(BestCand).

trans(P, S, P1, S1) :-
    H = horizon(S),
    trans(H, P, S, _H1, P1, S1).

:- pred trans_max_h(horizon, prog(A, B, P), sit(A),
                    sit(A), int) <= bat(A, B, P).
:- mode trans_max_h(in, in(semidet_prog), in,
                    out, out) is det.

trans_max_h(H, P, S, S2, SuccTrans2) :-
    if      H = 0 ; final(H, P, S)
    then    S2 = S, SuccTrans2 = 0
    else if trans(H, P, S, H1, P1, S1)
    then    trans_max_h(H1, P1, S1, S2, SuccTrans1),
            SuccTrans2 = SuccTrans1 + 1
    else    S2 = S, SuccTrans2 = 0.

:- pred final(horizon, prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode final(in, in(semidet_prog), in) is semidet.

final(H, P, S) :-
    maybe_final(P),
    not (
        next(P, C, R),
        trans_max_h(H, seq(pseudo_atom(C), R), S, S1, _SuccTrans),
        reward(S1) > reward(S)
    ).


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
% Note that the horizon must be positive and reward non-negative.
%
% The core predicates are:
% * next/2 to decompose a program into one next atomic action its remainder,
% * next2/2 to resolve complex atomic actions,
% * trans_atom/3 to execute primitive, stochastic (sampling), and test actions,
% * trans/4 and trans/6 to pick one decomposition and execute its next step,
% * final/2 and final/3 that decide whether or not a program is final,
% * do/3 that executes a program until it's final.
%
% The interpreter features sequence, recursive procedure calls, nondeterministic
% branch, nondeterministic loop, concurrency through nondeterministic
% interleaving, sequence, test actions, primitive actions, stochastic actions,
% and atomic complex actions.
% Nondeterminism is resolved by choosing the alternative that maximizes a
% reward after a lookahead horizon defined in the BAT.
%
% Fluent evaluation is not implemented with an interpreting predicate holds/2
% as in Prolog, but using higher-order predicates which can be combined with
% the predicates and/3, or/3 etc. defined below.
%
% The pick operator is not implemented. It seems to be difficult to do so:
% Higher-order terms apparently cannot contain unbound variables, and I
% don't know how to inspect and modify terms.
% I guess it is pretty difficult to express this in the type system. Maybe
% one needs the univ type and/or the deconstruct module or so.
%
% Christoph Schwering (schwering@gmail.com)

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


:- pred eq(funfluent(A, R), funfluent(A, R), sit(A)).
:- mode eq(in(func(in) = out is semidet),
           in(func(in) = out is semidet), in) is semidet.

:- pred and(relfluent(A), relfluent(A), sit(A)).
:- mode and(in(pred(in) is semidet), in(pred(in) is semidet), in) is semidet.

:- pred or(relfluent(A), relfluent(A), sit(A)).
:- mode or(in(pred(in) is semidet), in(pred(in) is semidet), in) is semidet.

:- pred neg(relfluent(A), sit(A)).
:- mode neg(in(pred(in) is semidet), in) is semidet.


:- pred trans(prog(A, B, P), sit(A), prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode trans(in(semidet_prog), in, out(semidet_prog), out) is semidet.

:- pred do(prog(A, B, P), sit(A), sit(A)) <= bat(A, B, P).
:- mode do(in(semidet_prog), in, out) is semidet.

:- pred final(prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode final(in(semidet_prog), in) is semidet.


:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.


eq(Lhs, Rhs, S) :- Lhs(S) = Rhs(S).
and(T1, T2, S) :- T1(S), T2(S).
or(T1, T2, S) :- T1(S) ; T2(S).
neg(T, S) :- not T(S).


:- pred next(prog(A, B, P), pseudo_atom(A, B, P), prog(A, B, P))
    <= bat(A, B, P).
:- mode next(in(semidet_prog), out(semidet_pseudo_atom), out(semidet_prog))
    is nondet.

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


:- pred maybe_final(prog(A, B, P)) <= bat(A, B, P).
:- mode maybe_final(in(semidet_prog)) is semidet.

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


:- pred next2(prog(A, B, P), atom(A, B), prog(A, B, P)) <= bat(A, B, P).
:- mode next2(in(semidet_prog), out(semidet_atom), out(semidet_prog)) is nondet.

next2(P, C, R) :-
    next(P, C1, R1),
    (   C1 = complex(P1),
        next2(P1, C, R0),
        R = seq(R0, R1)
    ;   C1 = atom(C),
        R = R1
    ).


:- pred trans_atom(atom(A, B), sit(A), sit(A)) <= bat(A, B, P).
:- mode trans_atom(in(semidet_atom), in, out) is semidet.

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


:- type decomposition(A, B, P) ---> part(head :: atom(A, B),
                                         rest :: prog(A, B, P)).
:- inst semidet_decomposition ---> part(semidet_atom, semidet_prog).

:- type candidate(A, B, P) ---> candidate(rest_horizon :: horizon,
                                          prog         :: prog(A, B, P),
                                          sit          :: sit(A),
                                          value        :: reward,
                                          success      :: int).
:- inst semidet_candidate ---> candidate(ground,
                                         semidet_prog,
                                         ground,
                                         ground,
                                         ground).

:- pred trans(horizon, prog(A, B, P), sit(A),
              horizon, prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode trans(in, in(semidet_prog), in,
              out, out(semidet_prog), out) is semidet.

trans(H, P, S, H1, P1, S1) :-
    H >= 1,
    % Determine all possible decompositions.
    (pred(MyDecomps::out(list(semidet_decomposition))) is det :-
        solutions((pred(MyDecomp::out(semidet_decomposition)) is nondet :-
            next2(P, head(MyDecomp), rest(MyDecomp))
        ), MyDecomps)
    )(Decomps),
    % If there is only one decomposition, we don't need to look ahead.
    % Otherwise, we look ahead H transitions and pick that decomposition which
    % maximizes the reward function.
    (   if      Decomps = [Decomp]
        then    trans_atom(head(Decomp), S, S1),
                P1 = rest(Decomp),
                H1 = new_horizon(H, head(Decomp))
        else    InitCand = candidate(-1, nil, s0, -1, -1),
                list.foldl((pred(Decomp::in(semidet_decomposition),
                                 Cand1::in(semidet_candidate),
                                 Better::out(semidet_candidate)) is det :-
                    if      trans_atom(head(Decomp), S, sit(Cand2)),
                            rest_horizon(Cand2) = new_horizon(H, head(Decomp)),
                            prog(Cand2) = rest(Decomp),
                            trans_max_h(rest_horizon(Cand2), prog(Cand2),
                                        sit(Cand2), S2, success(Cand2)),
                            value(Cand2) = reward(S2),
                            (   value(Cand2) > value(Cand1)
                            ;   value(Cand2) = value(Cand1),
                                success(Cand2) > success(Cand1))
                    then    Better = Cand2
                    else    Better = Cand1
                ), Decomps, InitCand, BestCand),
                BestCand \= InitCand,
                H1 = rest_horizon(BestCand),
                S1 = sit(BestCand),
                P1 = prog(BestCand)
    ).


trans(P, S, P1, S1) :-
    H = horizon(S),
    trans(H, P, S, _H1, P1, S1).


:- pred trans_max_h(horizon, prog(A, B, P), sit(A), sit(A), int)
    <= bat(A, B, P).
:- mode trans_max_h(in, in(semidet_prog), in, out, out) is det.

trans_max_h(H, P, S, S2, Success2) :-
    if      H = 0 ; final(H, P, S)
    then    S2 = S, Success2 = 2*H*H  % rate sooner final better than late final
    else if trans(H, P, S, H1, P1, S1)
    then    trans_max_h(H1, P1, S1, S2, Success1),
            Success2 = Success1 + 1
    else    S2 = S, Success2 = 0.


do(P, S, S2) :-
    if      final(P, S)
    then    S = S2
    else    trans(P, S, P1, S1),
            do(P1, S1, S2).


:- pred final(horizon, prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode final(in, in(semidet_prog), in) is semidet.

final(H, P, S) :-
    maybe_final(P),
    not (
        next(P, C, R),
        trans_max_h(H, seq(pseudo_atom(C), R), S, S1, _Success),
        reward(S1) > reward(S)
    ).


final(P, S) :-
    final(horizon(S), P, S).


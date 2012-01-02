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
:- type horizon == int.

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

    func reward(sit(A)) = reward,
    mode reward(in) = out is det,

    func horizon(sit(A)) = horizon,
    mode horizon(in) = out is det,

    func new_horizon(horizon, atom(A, B)) = horizon,
    mode new_horizon(in, in) = out is det,

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


:- pred next(prog(A, B, P), pseudo_atom(A, B, P), prog(A, B, P))
    <= bat(A, B, P).
:- mode next(in(prog), out(pseudo_atom), out(prog))
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
:- mode maybe_final(in(prog)) is semidet.

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
:- inst decomposition ---> part(atom, prog).

:- type candidate(A, B, P) ---> candidate(rest_horizon :: horizon,
                                          prog         :: prog(A, B, P),
                                          sit          :: sit(A),
                                          value        :: reward,
                                          success      :: int).
:- inst candidate ---> candidate(ground,
                                 prog,
                                 ground,
                                 ground,
                                 ground).

:- pred trans(horizon, prog(A, B, P), sit(A),
              horizon, prog(A, B, P), sit(A)) <= bat(A, B, P).
:- mode trans(in, in(prog), in,
              out, out(prog), out) is semidet.

trans(H, P, S, H1, P1, S1) :-
    H >= 1,
    % Determine all possible decompositions.
    (pred(MyDecomps::out(list(decomposition))) is det :-
        solutions((pred(MyDecomp::out(decomposition)) is nondet :-
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
                list.foldl((pred(Decomp::in(decomposition),
                                 Cand1::in(candidate),
                                 Better::out(candidate)) is det :-
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
:- mode trans_max_h(in, in(prog), in, out, out) is det.

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
:- mode final(in, in(prog), in) is semidet.

final(H, P, S) :-
    maybe_final(P),
    not (
        next(P, C, R),
        trans_max_h(H, seq(pseudo_atom(C), R), S, S1, _Success),
        reward(S1) > reward(S)
    ).


final(P, S) :-
    final(horizon(S), P, S).


% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0

:- module test.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.


:- implementation.

:- import_module prgolog.
:- import_module prgolog.fluent.
:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.
:- import_module term_io.


% We have six primitive actions aI and abI for I = 1,2,3.
% We have three stochastic actions bI, each of which as always the same
% outcome action abI (the reason is that I don't have looked for a
% random number generator for sampling).
% All actions are always possible.
% Have a close look at the reward function.

:- type prim_action ---> a1 ; a2 ; a3 ; ab1 ; ab2 ; ab3.
:- type stoch_action ---> b1 ; b2 ; b3.
:- type procedure ---> p1 ; p2 ; p3.


:- pred poss(prim_action::in, sit(prim_action)::in) is semidet.

poss(A, _S) :-
    (   A = a1  ; A = a2  ; A = a3
    ;   A = ab1 ; A = ab2 ; A = ab3
    ).


:- pred random_outcome(stoch_action::in, prim_action::out, S::in) is det.

random_outcome(B, A, _S) :-
    (   B = b1, A = ab1
    ;   B = b2, A = ab2
    ;   B = b3, A = ab3
    ).


:- func reward(sit(prim_action)) = int is det.

reward(S) = (
    if      S = do(a1,  S0) then min(10, test.reward(S0) + 1)
    else if S = do(a2,  S0) then test.reward(S0) + 0
    else if S = do(a3,  S0) then test.reward(S0) + 0
    else if S = do(ab1, S0) then test.reward(S0) + 0
    else if S = do(ab2, S0) then test.reward(S0) + 0
    else if S = do(ab3, S0) then test.reward(S0) + 4
    else                         0
).


:- func horizon(sit(prim_action)) = horizon is det.

horizon(_S) = 3.


:- func new_horizon(horizon, atom(prim_action, stoch_action)) = horizon is det.

new_horizon(H, C) = H1 :-
    (   C = prim(_),  H1 = max(H - 1, 0)
    ;   C = stoch(_), H1 = max(H - 1, 0)
    ;   C = test(_),  H1 = H              % ignore test actions
    ).


:- pred proc(procedure, prog(prim_action, stoch_action, procedure)).
:- mode proc(in(ground), out(prog)) is det.

proc(P, P1) :-
    (   P = p1, P1 = pseudo_atom(atom(prim(a1)))
    ;   P = p2, P1 = nil
    ;   P = p3, P1 = pseudo_atom(atom(stoch(b3)))
    ).


:- instance bat(test.prim_action, test.stoch_action, test.procedure) where [
    pred(poss/2) is test.poss,
    pred(random_outcome/3) is test.random_outcome,
    func(reward/1) is test.reward,
    func(horizon/1) is test.horizon,
    func(new_horizon/2) is test.new_horizon,
    pred(proc/2) is test.proc
].


:- pred fluent(sit(prim_action)).
:- mode fluent(in) is semidet.
:- pragma memo(fluent/1). 
fluent(S) :- S = s0.


:- pred fluent2(int, sit(prim_action)).
:- mode fluent2(out, in) is semidet.
fluent2(X, S) :- X = 1, S = s0.


main(!IO) :-
    % Due to the type system, many functors are needed to construct a program.
    % We use some helper variables to keep things clear.
    A1 = pseudo_atom(atom(prim(a1))),
    A2 = pseudo_atom(atom(prim(a2))),
    A3 = pseudo_atom(atom(prim(a3))),
    B1 = pseudo_atom(atom(stoch(b1))),
    B2 = pseudo_atom(atom(stoch(b2))),
    B3 = pseudo_atom(atom(stoch(b3))),
    Q1 = pseudo_atom(complex(A1 `seq` A2 `seq` A3)) `conc`
         pseudo_atom(complex(B1 `seq` B2 `seq` B3)),
    Q2 = Q1 `seq` (nil `non_det` A1), % final reward is 5
    Q3 = star(A1),                    % final reward is 10
    Q4 = pseudo_atom(atom(test(fluent `and` fluent))) `seq` (Q2 `non_det` Q3),
    (   if      do(Q4, s0, S1)
        then    io.format("ok\n", [], !IO),
                io.write(S1, !IO), io.nl(!IO),
                true% io.write(P1, !IO), io.nl(!IO)
        else    io.format("fail\n", [], !IO)
    ).


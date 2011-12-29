% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0

:- module main.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.


:- implementation.
:- import_module prgolog.
:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.
:- import_module term_io.

:- type prim_action ---> a1 ; a2 ; a3 ; ab1 ; ab2 ; ab3.
:- type stoch_action ---> b1 ; b2 ; b3.
:- type procedure ---> p1 ; p2 ; p3.

:- pred poss(prim_action::in, sit(prim_action)::in) is det.% semidet.
poss(_A, _S) :-
    true.

:- pred random_outcome(stoch_action::in, prim_action::out, S::in) is det.
random_outcome(B, A, _S) :-
    (   B = b1, A = ab1
    ;   B = b2, A = ab2
    ;   B = b3, A = ab3
    ).

:- func reward(sit(prim_action)) = int is det.
reward(S) = (
    if      S = do(a1, S0)  then main.reward(S0) + 0
    else if S = do(a2, S0)  then main.reward(S0) + 0
    else if S = do(a3, S0)  then main.reward(S0) + 0
    else if S = do(ab1, S0) then main.reward(S0) + 0
    else if S = do(ab2, S0) then main.reward(S0) + 0
    else if S = do(ab3, S0) then main.reward(S0) + 3
    else                         0
).

:- func horizon(sit(prim_action)) = horizon is det.
horizon(_S) = 3.

:- func new_horizon(horizon, atom(prim_action, stoch_action)) = horizon is det.
new_horizon(H, C) = (
    if      C = prim(a1)  then max(H - 1, 0)
    else if C = prim(a2)  then max(H - 1, 0)
    else if C = prim(a3)  then max(H - 1, 0)
    else if C = stoch(b1) then max(H - 1, 0)
    else if C = stoch(b2) then max(H - 1, 0)
    else                       max(H - 1, 0)
).

:- pred proc(procedure, prog(prim_action, stoch_action, procedure)).
:- mode proc(in(ground), out(semidet_prog)) is det.
proc(P, P1) :-
    (   P = p1, P1 = pseudo_atom(atom(prim(a1)))
    ;   P = p2, P1 = nil
    ;   P = p3, P1 = pseudo_atom(atom(stoch(b3)))
    ).


:- instance bat(main.prim_action, main.stoch_action, main.procedure) where [
    pred(poss/2) is main.poss,
    pred(random_outcome/3) is main.random_outcome,
    func(reward/1) is main.reward,
    func(horizon/1) is main.horizon,
    func(new_horizon/2) is main.new_horizon,
    pred(proc/2) is main.proc
].

:- pred p(int, int, int).
:- mode p(in, in, out) is semidet.
p(X, Y, Z) :- X = 1, Y + 1 = Z.

:- pred q(int, int, int).
:- mode q(in, in, out) is nondet.
q(X, Y, Z) :- X = 1, Y + 1 = Z ; X = 1, Y + 2 = Z.

:- pred a(pred(int), int).
:- mode a(pred(out) is semidet, out) is semidet.
:- mode a(pred(out) is nondet, out) is nondet.
a(P, Y) :- call(P, Y).

:- pred fluent(sit(prim_action)).
:- mode fluent(in) is semidet.
fluent(S) :- S = s0.

main(!IO) :-
    A1 = pseudo_atom(atom(prim(a1))),
    A2 = pseudo_atom(atom(prim(a2))),
    A3 = pseudo_atom(atom(prim(a3))),
    B1 = pseudo_atom(atom(stoch(b1))),
    B2 = pseudo_atom(atom(stoch(b2))),
    B3 = pseudo_atom(atom(stoch(b3))),
    P = pseudo_atom(complex(A1 `seq` A2 `seq` A3)) `conc`
        pseudo_atom(complex(B1 `seq` B2 `seq` B3)),
    (   if      trans(pseudo_atom(atom(test(fluent))), s0, _P0, S0),
                trans(P, S0, P1, S1),
                true% S1 = do(a3, s0)
        then    io.format("ok\n", [], !IO), io.write(S1, !IO), io.nl(!IO), io.write(P1, !IO), io.nl(!IO)
        else    io.format("fail\n", [], !IO)
    ).


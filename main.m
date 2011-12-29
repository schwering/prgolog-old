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

:- type prim_action ---> a1 ; a2 ; a3.
:- type stoch_action ---> b1 ; b2 ; b3.
:- type procedure ---> p1 ; p2 ; p3.

:- pred poss(prim_action::in, sit(prim_action)::in) is det.% semidet.
poss(_A, _S) :-
    true.

:- pred random_outcome(stoch_action::in, prim_action::out, S::in) is det.
random_outcome(B, A, _S) :-
    (   B = b1, A = a1
    ;   B = b2, A = a2
    ;   B = b3, A = a3
    ).

:- func reward(sit(prim_action)) = int is det.
reward(S) = (
    if      S = do(a1, S0) then main.reward(S0) + 1
    else if S = do(_,  S0) then main.reward(S0)
    else                        0
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

main(!IO) :-
    if      P = nil `seq` pseudo_atom(atom(prim(a1))) `seq` proc(p1) `seq` pseudo_atom(atom(stoch(b3))),
            trans(P, s0, P1, S1),
            S1 = do(a1, s0)
    then    io.format("ok\n", [], !IO), io.write(S1, !IO), io.nl(!IO), io.write(P1, !IO), io.nl(!IO)
    else    io.format("fail\n", [], !IO).


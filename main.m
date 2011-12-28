% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0

:- module main.

:- interface.

:- import_module prgolog.
:- import_module io.
:- pred main(io::di, io::uo) is det.


:- implementation.
:- import_module list.
:- import_module solutions.
:- import_module string.

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
    if      S = do(a1, S0) then main.reward(S0)% + 1
    else if S = do(_,  S0) then main.reward(S0)
    else                        0
).

:- pred proc(procedure::in, prog(prim_action, stoch_action, procedure)::out) is det.
proc(P, P1) :-
    (   P = p1, P1 = pseudo_atom(atom(prim(a1)))
    ;   P = p2, P1 = nil
    ;   P = p3, P1 = pseudo_atom(atom(prim(a3)))
    ).


:- instance bat(prim_action, stoch_action, procedure) where [
    pred(poss/2) is main.poss,
    pred(random_outcome/3) is main.random_outcome,
    func(reward/1) is main.reward,
    pred(proc/2) is main.proc
].


main(!IO) :-
    if      trans(proc(p1), s0, _P1, S1), S1 = do(a1, s0)
    then    io.format("ok\n", [], !IO)
    else    io.format("na, du?\n", [], !IO).


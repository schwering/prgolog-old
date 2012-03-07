% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0

:- module main.

:- interface.

:- import_module io.

:- pred main(io.io::di, io.io::uo) is det.


:- implementation.

:- import_module skat.
:- import_module solutions.
:- import_module list.

main(!IO) :-
    G = suit_game(hearts),
    C = card(clubs, jack),
    length(S, L),
    solutions((pred(C1::out) is nondet :- tricks(G, C, C1)), S),
    write(S, !IO), nl(!IO),
    write(L, !IO), nl(!IO),
    true.


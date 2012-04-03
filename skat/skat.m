% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0

:- module skat.

:- interface.

:- type game ---> suit_game(suit) ; grand ; null.
:- type suit ---> clubs ; spades ; hearts ; diamonds.
:- type rank ---> ace ; king ; queen ; jack ; ten ; nine ; eight ; seven.
:- type card ---> card(suit :: suit, rank :: rank).

:- pred is_trump(game, card).
:- mode is_trump(in, in) is semidet.
:- mode is_trump(in, out) is nondet.

:- pred tricks(game, card, card).
:- mode tricks(in, in, in) is semidet.
:- mode tricks(in, in, out) is nondet.


:- implementation.

:- import_module(list).


:- func all_suits = {suit, suit, suit, suit}.
:- mode all_suits = out.
:- func all_suits_but(suit) = {suit, suit, suit}.
:- mode all_suits_but(in) = out.

all_suits = {clubs, spades, hearts, diamonds}.

all_suits_but(clubs) = {spades, hearts, diamonds}.
all_suits_but(spades) = {clubs, hearts, diamonds}.
all_suits_but(hearts) = {clubs, spades, diamonds}.
all_suits_but(diamonds) = {clubs, spades, hearts}.

:- pred some_card(card).
:- mode some_card(in) is det.
:- mode some_card(out) is multi.

some_card(card(S, R)) :-
    ( S = clubs ; S = spades ; S = hearts ; S = diamonds ),
    ( R = ace ; R = king ; R = queen ; R = jack
    ; R = ten ; R = nine ; R = eight ; R = seven ).


:- func rank_value(rank) = int.
:- mode rank_value(in) = out is det.
:- mode rank_value(out) = out is multi.

rank_value(ace)   = 11.
rank_value(ten)   = 10.
rank_value(king)  =  4.
rank_value(queen) =  3.
rank_value(jack)  =  2.
rank_value(nine)  =  0.
rank_value(eight) =  0.
rank_value(seven) =  0.


:- func default_trick_order_jack = list(card) is det.
:- func default_trick_order_nonjack(suit) = list(card) is det.
:- func inline_trick_order_nonjack(suit) = list(card) is det.

default_trick_order_jack =
    map((func(S) = card(S, jack)), [S1, S2, S3, S4]) :-
    {S1, S2, S3, S4} = all_suits.

default_trick_order_nonjack(S) =
    map((func(R) = card(S, R)), [ace, ten, king, queen, nine, eight, seven]).

inline_trick_order_nonjack(S) =
    map((func(R) = card(S, R)), [ace, king, queen, ten, nine, eight, seven]).


:- func trick_order(game) = list(card) is det.
%:- pragma memo(trick_order/1, [allow_reset, fast_loose]).

trick_order(suit_game(S)) =
    default_trick_order_jack ++
    default_trick_order_nonjack(S) ++
    default_trick_order_nonjack(S1) ++
    default_trick_order_nonjack(S2) ++
    default_trick_order_nonjack(S3) :-
    {S1, S2, S3} = all_suits_but(S).
trick_order(grand) =
    default_trick_order_jack ++
    default_trick_order_nonjack(S1) ++
    default_trick_order_nonjack(S2) ++
    default_trick_order_nonjack(S3) ++
    default_trick_order_nonjack(S4) :-
    {S1, S2, S3, S4} = all_suits.
trick_order(null) =
    default_trick_order_jack ++
    inline_trick_order_nonjack(S1) ++
    inline_trick_order_nonjack(S2) ++
    inline_trick_order_nonjack(S3) ++
    inline_trick_order_nonjack(S4) :-
    {S1, S2, S3, S4} = all_suits.


%:- pragma memo(is_trump/2, [allow_reset, fast_loose]). 

is_trump(suit_game(S), C) :-
    some_card(C), % do this here, otherwise no backtracking (why?)
    ( rank(C) = jack ; rank(C) \= jack, suit(C) = S ).
is_trump(grand, C) :-
    some_card(C), % do this here, otherwise no backtracking (why?)
    rank(C) = jack.
is_trump(null, C) :-
    some_card(C), % do this here, otherwise no backtracking (why?)
    rank(C) = jack.



%:- pragma memo(tricks/3, [allow_reset, fast_loose]). 

tricks(G, C1, C2) :-
    Order = trick_order(G),
    ( suit(C1) = suit(C2) ; is_trump(G, C1) ),
    member(C1, Order, [_|Tricked]),
    member(C2, Tricked).


%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: util.rand.test.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module util.rand.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test_random1(io::di, io::uo) is det.
:- pred test_random2(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module list.
:- import_module string.
:- import_module require.
:- import_module std_util.

%-----------------------------------------------------------------------------%

test_random1(!IO) :-
    some [!RandomSupply] (
        init(10, !:RandomSupply),
        random_float(R0, !RandomSupply),
        (   if succeed_with_prob(0.0, yes, !RandomSupply)
            then throw({"succeeded with 0.0", !.RandomSupply}) else true ),
        random_float(R1, !RandomSupply),
        (   if succeed_with_prob(1.0, yes, !RandomSupply)
            then true else throw({"failed with 1.0", !.RandomSupply}) ),
        random_float(R2, !RandomSupply),
        (   if succeed_with_prob(0.5, yes, !RandomSupply)
            then true else true ),
        random_float(R3, !RandomSupply),
        (   if succeed_with_prob(0.5, yes, !RandomSupply)
            then true else true ),
        random_float(R4, !.RandomSupply, _),
        (   if  R0 \= R1, R0 \= R2, R0 \= R3, R0 \= R4,
                R1 \= R2, R1 \= R3, R1 \= R4,
                R2 \= R3, R2 \= R4,
                R3 \= R4
            then true else throw({"got duplicate random, probably error",
                                 R0, R1, R2, R3, R4}) ),
        true
    ).


test_random2(!IO) :-
    some [!RandomSupply] (
        init(10, !:RandomSupply),
        some [Lo, Hi, X] ( {Lo, Hi} = {0.0, 1.0}, random_float({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {-10.0, 1.0}, random_float({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {-1.0, 1.0}, random_float({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {-1.0, 0.0}, random_float({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {50.0, 100.0}, random_float({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {50.0, 50.0}, random_float({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {-50.0, -50.0}, random_float({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) ))
    ),
    true.

%-----------------------------------------------------------------------------%
:- end_module util.rand.test.
%-----------------------------------------------------------------------------%

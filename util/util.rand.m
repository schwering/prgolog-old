%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: util.rand.m.
% Main author: schwering.
%
% Wrapper for random number generator of the standard library.
%
% This module adds some general-purpose functions to generate floats and
% a typeclass with some simple implementations.  With this typeclass, sampling-
% based algorithms can implemented in a more generic way.
%
%-----------------------------------------------------------------------------%

:- module util.rand.

:- interface.

:- import_module bool.
:- use_module random.

%-----------------------------------------------------------------------------%

:- type supply == random.supply.
:- type random_supply == supply.

:- type bounds(T) == {T, T}.

%-----------------------------------------------------------------------------%

:- pred init(int::in, supply::muo) is det.

:- pred succeed_with_prob(float::in, bool::out,
                          supply::mdi, supply::muo) is det.

:- pred random_int(int::out, supply::mdi, supply::muo) is det.

:- pred random_int(bounds(int)::in, int::out, supply::mdi, supply::muo) is det.

:- pred max_random_int(int::out, supply::mdi, supply::muo) is det.

:- pred random_float(float::out, supply::mdi, supply::muo) is det.

:- pred random_float(bounds(float)::in, float::out,
               supply::mdi, supply::muo) is det.

%-----------------------------------------------------------------------------%

:- typeclass random_generator(T) where [
    pred random(T, supply, supply),
    mode random(out, mdi, muo) is det,

    pred random(bounds(T), T, supply, supply),
    mode random(in, out, mdi, muo) is det
].

:- instance random_generator(int).
:- instance random_generator(float).
:- instance random_generator({T1, T2}) <= (random_generator(T1),
                                           random_generator(T2)).
:- instance random_generator({T1, T2, T3}) <= (random_generator(T1),
                                               random_generator(T2),
                                               random_generator(T3)).

%-----------------------------------------------------------------------------%

:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.

%-----------------------------------------------------------------------------%

init(Seed, Supply) :-
    random.init(Seed, Supply).

%-----------------------------------------------------------------------------%

succeed_with_prob(Probability, Outcome, !RandomSupply) :-
    random_int({0, 100}, Random, !RandomSupply),
    Event = float(Random) / 100.0,
    Outcome = ( if Event < Probability then yes else no ).

%-----------------------------------------------------------------------------%

random_int(Outcome, !RandomSupply) :-
    random.random(Outcome, !RandomSupply).

random_int({Lo, Hi}, Outcome, !RandomSupply) :-
    random.random(Lo, Hi, Outcome, !RandomSupply).

max_random_int(Max, !RandomSupply) :-
    random.randmax(Max, !RandomSupply).

%-----------------------------------------------------------------------------%

random_float(Outcome, !RandomSupply) :-
    random_int(Numer, !RandomSupply),
    max_random_int(Denom, !RandomSupply),
    Outcome = float(Numer) / float(Denom).


random_float({Lo, Hi}, Outcome, !RandomSupply) :-
    random_float(Outcome0, !RandomSupply),
    Outcome = Outcome0 * (Hi - Lo) + Lo.

%-----------------------------------------------------------------------------%

:- instance random_generator(int) where [
    pred(random/3) is random_int,
    pred(random/4) is random_int
].

:- instance random_generator(float) where [
    pred(random/3) is random_float,
    pred(random/4) is random_float
].

:- instance random_generator({T1, T2}) <= (random_generator(T1),
                                           random_generator(T2)) where [
    (random({O1, O2}, !RandomSupply) :-
        random(O1, !RandomSupply),
        random(O2, !RandomSupply)
    ),
    (random({ {Lo1, Lo2}, {Hi1, Hi2} }, {O1, O2}, !RandomSupply) :-
        random({Lo1, Hi1}, O1, !RandomSupply),
        random({Lo2, Hi2}, O2, !RandomSupply)
    )
].

:- instance random_generator({T1, T2, T3}) <= (random_generator(T1),
                                               random_generator(T2),
                                               random_generator(T3)) where [
    (random({O1, O2, O3}, !RandomSupply) :-
        random(O1, !RandomSupply),
        random(O2, !RandomSupply),
        random(O3, !RandomSupply)
    ),
    (random({ {Lo1, Lo2, Lo3},
              {Hi1, Hi2, Hi3} },
              {O1,  O2,  O3}, !RandomSupply) :-
        random({Lo1, Hi1}, O1, !RandomSupply),
        random({Lo2, Hi2}, O2, !RandomSupply),
        random({Lo3, Hi3}, O3, !RandomSupply)
    )
].

%-----------------------------------------------------------------------------%
:- end_module util.rand.
%-----------------------------------------------------------------------------%

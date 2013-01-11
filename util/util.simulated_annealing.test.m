%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: util.simulated_annealing.test.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module util.simulated_annealing.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test_random(io::di, io::uo) is det.
:- pred test_sa(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module list.
:- import_module string.
:- import_module util.rat.
:- import_module require.
:- import_module std_util.

%-----------------------------------------------------------------------------%

test_random(!IO) :-
    some [!RandomSupply] (
        random.init(10, !:RandomSupply),
        random.random(R0, !RandomSupply),
        (   if succeed_with_prob(0.0, yes, !RandomSupply)
            then throw({"succeeded with 0.0", !.RandomSupply}) else true ),
        random.random(R1, !RandomSupply),
        (   if succeed_with_prob(1.0, yes, !RandomSupply)
            then true else throw({"failed with 1.0", !.RandomSupply}) ),
        random.random(R2, !RandomSupply),
        (   if succeed_with_prob(0.5, yes, !RandomSupply)
            then true else true ),
        random.random(R3, !RandomSupply),
        (   if succeed_with_prob(0.5, yes, !RandomSupply)
            then true else true ),
        random.random(R4, !.RandomSupply, _),
        (   if  R0 \= R1, R0 \= R2, R0 \= R3, R0 \= R4,
                R1 \= R2, R1 \= R3, R1 \= R4,
                R2 \= R3, R2 \= R4,
                R3 \= R4
            then true else throw({"got duplicate random, probably error",
                                 R0, R1, R2, R3, R4}) ),
        true
    ).


test_sa(!IO) :-
    InitialState = -1.0,
    Schedule = (func(Time) = float.max(0.0, 1000.0 - float(Time))),
    Next = (func(X) = X + 0.1),
    Value = (func(X) = -1.0 * abs(X*X*X + X*X*X*X + 0.1)),
    simulated_annealing(10, Schedule, Next, Value, InitialState, Result),
    format("Result = %f\n", [f(Result)], !IO),
    true.

%-----------------------------------------------------------------------------%
:- end_module util.simulated_annealing.test.
%-----------------------------------------------------------------------------%

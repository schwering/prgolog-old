%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: simulated_annealing.m.
% Main author: schwering.
%
% Simulated annealing.
%
%-----------------------------------------------------------------------------%

:- module simulated_annealing.

:- interface.

%-----------------------------------------------------------------------------%

:- use_module random.

%-----------------------------------------------------------------------------%

:- type time == int.
:- type temperature == float.
:- type schedule_func == (func(time) = temperature).
:- type next_func(T) == (func(T) = T).
:- type value_func(T) == (func(T) = float).

%-----------------------------------------------------------------------------%

:- pred simulated_annealing(
    int::in, schedule_func::in, next_func(T)::in, value_func(T)::in,
    T::in, T::out) is det.

    % simulated_annealing(Schedule, Next, Value, !Time, !State, !Random):
    % Computes the next state Next(!.State) and proceeds with that one if it's
    % an improvement wrt Value; otherwise it tosses a coin and continues with
    % probability exp(Delta / Temperature) where
    %  - Delta is the (negative) difference between the new and old state
    %  - Temperature is Schedule(!.Time).
    % !Time is simply a counter incremented by one by each invocation.
    %
:- pred simulated_annealing(
    schedule_func::in, next_func(T)::in, value_func(T)::in,
    time::in, time::out, T::in, T::out,
    random.supply::in, random.supply::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.
:- use_module math.

%-----------------------------------------------------------------------------%

simulated_annealing(Seed, Schedule, Next, Value, !State) :-
    random.init(Seed, Random),
    simulated_annealing(Schedule, Next, Value, 0, _, !State, Random, _).

%-----------------------------------------------------------------------------%

:- pred succeed_with_probability(float::in, random.supply::in,
                                            random.supply::out) is semidet.

succeed_with_probability(Probability, !RandomSupply) :-
    random.random(0, 100, Random, !RandomSupply),
    Event = float(Random) / 100.0,
    Event < Probability.


simulated_annealing(Schedule, Next, Value, !Time, !State, !Random) :-
    Temperature = Schedule(!.Time),
    S = Next(!.State),
    Delta = Value(S) - Value(!.State),
    (   if      Delta > 0.0
        then    !:State = S
        else if succeed_with_probability(math.exp(Delta / Temperature), !Random)
        then    !:State = S
        else    !:State = !.State
    ),
    !:Time = !.Time + 1.

%-----------------------------------------------------------------------------%
:- end_module simulated_annealing.
%-----------------------------------------------------------------------------%

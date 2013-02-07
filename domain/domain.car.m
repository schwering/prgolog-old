%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.m.
% Main author: schwering.
%
% Basic types used throughout the cars world.
%
%-----------------------------------------------------------------------------%

:- module domain.car.

:- interface.

:- import_module list.
:- import_module prgolog.

:- type agent ---> b ; c ; d ; e ; f ; g ; h.
:- type lane ---> left ; right.

:- type degree == float.
:- type rad == float.
:- type kmph == float.
:- type mps == float.
:- type mpss == float.
:- type m == float.
:- type s == float.

:- type pos ---> p(x :: m, y :: m).

:- type car_obs ---> some [T] ( car_obs(T) => car_obs(T) ).

%-----------------------------------------------------------------------------%

    % Typeclass for observations.
    % We could define the ternary functions that compute agent-to-agent
    % information like x_dist/3, y_dist/3, veloc_diff/3 as general functions,
    % but we don't to allow faster implementations.
    % Beware of the signs!
:- typeclass car_obs(Obs) where [
    func time(Obs) = s,
    mode time(in) = out is det,

    func veloc(Obs, agent) = mps,
    mode veloc(in, in) = out is semidet,

    func yaw(Obs, agent) = rad,
    mode yaw(in, in) = out is semidet,

    func pos(Obs, agent) = pos,
    mode pos(in, in) = out is semidet,

    func x_pos(Obs, agent) = m,
    mode x_pos(in, in) = out is semidet,

    func y_pos(Obs, agent) = m,
    mode y_pos(in, in) = out is semidet,

        % x_dist(Obs, B, C): x_B - x_C
    func x_dist(Obs, agent, agent) = m,
    mode x_dist(in, in, in) = out is semidet,

        % y_dist(Obs, B, C): y_B - y_C
    func y_dist(Obs, agent, agent) = m,
    mode y_dist(in, in, in) = out is semidet,

        % veloc_diff(Obs, B, C): v_B - v_C
    func veloc_diff(Obs, agent, agent) = m,
    mode veloc_diff(in, in, in) = out is semidet,

        % net_time_gap(Obs, B, C): (x_C - x_B) / v_B
    func net_time_gap(Obs, agent, agent) = m,
    mode net_time_gap(in, in, in) = out is semidet,

        % time_to_collision(Obs, B, C): (x_C - x_B) / (v_B - v_C)
    func time_to_collision(Obs, agent, agent) = m,
    mode time_to_collision(in, in, in) = out is semidet
].

    % Actually we don't need this anywhere. The element of car_obs(_) implements
    % the car_obs typeclass anyway. This wrapping implementation isn't very
    % elegant, so let's drop it.
%:- instance car_obs(car_obs).

%-----------------------------------------------------------------------------%

:- func deg2rad(degree::in) = (rad::out) is det.
:- func rad2deg(rad::in) = (degree::out) is det.
:- func kmh2ms(kmph::in) = (mps::out) is det.
:- func ms2kmh(kmph::in) = (mps::out) is det.

:- func deg_zero = (degree::out) is det.
:- func deg_min = (degree::out) is det.
:- func deg_max = (degree::out) is det.

%-----------------------------------------------------------------------------%

:- pred agent(agent).
:- mode agent(out) is multi.

:- func agent_to_string(agent) = string.
:- mode agent_to_string(in) = out is det.
:- mode agent_to_string(out) = in is semidet.

:- func string_to_agent(string) = agent.

:- func lane_to_string(lane) = string.

:- func agent_to_index(agent) = int.

:- func agents = list(agent).
:- func agent_pairs = list({agent, agent}).

%-----------------------------------------------------------------------------%

:- include_module cont.
:- include_module obs.
:- include_module rstc.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.
:- import_module math.
:- import_module require.
:- import_module solutions.
:- import_module string.

%-----------------------------------------------------------------------------%

:- pragma memo(agents/0, [fast_loose]). 
:- pragma memo(agent_pairs/0, [fast_loose]). 

%-----------------------------------------------------------------------------%

deg2rad(Deg) = Deg / 180.0 * pi.
rad2deg(Rad) = Rad * 180.0 / pi.
kmh2ms(Kmh) = Kmh / 3.6.
ms2kmh(Ms) = Ms * 3.6.

deg_zero = 0.0.
deg_min = deg_zero - 25.0.
deg_max = deg_zero - 25.0.

%-----------------------------------------------------------------------------%

agent(b).
agent(c).
agent(d).
agent(e).
agent(f).
agent(g).
agent(h).

agent_to_string(b) = "b".
agent_to_string(c) = "c".
agent_to_string(d) = "d".
agent_to_string(e) = "e".
agent_to_string(f) = "f".
agent_to_string(g) = "g".
agent_to_string(h) = "h".

string_to_agent(S) = A :-
    (   if      S = agent_to_string(A0)
        then    A = A0
        else    error("string_to_agent/1: conversion failed for '" ++ S ++ "'")
    ).


agent_to_index(b) = 0.
agent_to_index(c) = 1.
agent_to_index(d) = 2.
agent_to_index(e) = 3.
agent_to_index(f) = 4.
agent_to_index(g) = 5.
agent_to_index(h) = 6.

lane_to_string(left) = "left".
lane_to_string(right) = "right".


agents = solutions(agent).


agent_pairs = cross_product(agents, agents).


:- func cross_product(list(T1), list(T2)) = list({T1, T2}).

cross_product([], _) = [].
cross_product([X|Xs], Ys) = map(func(Y) = {X, Y}, Ys) ++ cross_product(Xs, Ys).

%-----------------------------------------------------------------------------%

%:- instance car_obs(car_obs) where [
%    (time(car_obs(Obs)) = time(Obs)),
%    (veloc(car_obs(Obs), B) = veloc(Obs, B)),
%    (yaw(car_obs(Obs), B) = yaw(Obs, B)),
%    (pos(car_obs(Obs), B) = pos(Obs, B)),
%    (x_pos(car_obs(Obs), B) = x_pos(Obs, B)),
%    (y_pos(car_obs(Obs), B) = y_pos(Obs, B)),
%    (x_dist(car_obs(Obs), B, C) = x_dist(Obs, B, C)),
%    (y_dist(car_obs(Obs), B, C) = y_dist(Obs, B, C)),
%    (veloc_diff(car_obs(Obs), B, C) = veloc_diff(Obs, B, C)),
%    (net_time_gap(car_obs(Obs), B, C) = net_time_gap(Obs, B, C)),
%    (time_to_collision(car_obs(Obs), B, C) = time_to_collision(Obs, B, C))
%].

%-----------------------------------------------------------------------------%
:- end_module domain.car.
%-----------------------------------------------------------------------------%

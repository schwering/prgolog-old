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

:- import_module assoc_list.
:- import_module prgolog.

:- type agent ---> b ; c ; d.
:- type lane ---> left ; right.

:- type degree == float.
:- type rad == float.
:- type kmph == float.
:- type mps == float.
:- type mpss == float.
:- type m == float.
:- type s == float.

:- type pos ---> p(x :: m, y :: m).
:- type info ---> info(veloc :: mps, rad :: rad, pos :: pos).
:- type env ---> env(s, assoc_list(agent, info)).
:- type obs ---> obs(s, assoc_list(agent, info)).

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

:- func string_to_agent(string) = agent is det.

:- func lane_to_string(lane) = string is det.

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
:- import_module string.

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

agent_to_string(b) = "b".
agent_to_string(c) = "c".
agent_to_string(d) = "d".

string_to_agent(S) = A :-
    (   if      S = agent_to_string(A0)
        then    A = A0
        else    error("string_to_agent/1: conversion failed for '" ++ S ++ "'")
    ).


lane_to_string(left) = "left".
lane_to_string(right) = "right".

%-----------------------------------------------------------------------------%
:- end_module domain.car.
%-----------------------------------------------------------------------------%

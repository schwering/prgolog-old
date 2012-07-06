%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: types.m.
% Main author: schwering.
%
% Basic types used throughout the cars world.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module types.

:- interface.

:- import_module assoc_list.
:- import_module prgolog.

:- type agent ---> a ; b.
:- type lane ---> left ; right.

:- type degree == float.
:- type rad == float.
:- type kmph == float.
:- type mps == float.
:- type mpss == float.
:- type m == float.
:- type s == float.

:- type agent_info ---> agent_info(mps, rad, m, m).

:- type obs == {s, agent, m, m, agent, m, m}.

%-----------------------------------------------------------------------------%

:- typeclass obs_bat(A, B, P) <= bat(A, B, P) where [
    pred is_match_action(A),
    mode is_match_action(in) is semidet,

    pred covered_by_match(sit(A)),
    mode covered_by_match(in) is semidet,

    func obs_to_match(obs) = A,
    mode obs_to_match(in) = out is det
].

%-----------------------------------------------------------------------------%

:- typeclass pr_bat(A, B, P) <= obs_bat(A, B, P) where [
    func seed_init_sit(int) = sit(A),
    mode seed_init_sit(in) = out is det,

    func init_env_sit(s, assoc_list(agent, agent_info), sit(A)) = sit(A),
    mode init_env_sit(in, in, in) = out is det
].

%-----------------------------------------------------------------------------%

:- func deg2rad(degree::in) = (rad::out) is det.
:- func rad2deg(rad::in) = (degree::out) is det.
:- func kmh2ms(kmph::in) = (mps::out) is det.
:- func ms2kmh(kmph::in) = (mps::out) is det.

:- func deg_zero = (degree::out) is det.
:- func deg_min = (degree::out) is det.
:- func deg_max = (degree::out) is det.

%-----------------------------------------------------------------------------%

:- func agent_to_string(agent) = string is det.
:- func string_to_agent(string) = agent is det.

:- func lane_to_string(lane) = string is det.

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

agent_to_string(a) = "a".
agent_to_string(b) = "b".

string_to_agent(S) = A :-
    if      S = agent_to_string(a)
    then    A = a
    else if S = agent_to_string(b)
    then    A = b
    else    error("string_to_agent/1: conversion failed for '" ++ S ++ "'").


lane_to_string(left) = "left".
lane_to_string(right) = "right".

%-----------------------------------------------------------------------------%
:- end_module types.
%-----------------------------------------------------------------------------%

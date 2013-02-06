%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.obs.env.m.
% Main author: schwering.
%
% Very simple implementation of car_obs typeclass.
%
%-----------------------------------------------------------------------------%

:- module domain.car.obs.env.

:- interface.

%-----------------------------------------------------------------------------%

:- import_module assoc_list.

%-----------------------------------------------------------------------------%

:- type info ---> info(veloc :: mps, yaw :: rad, pos :: pos).
:- type env ---> env(s, assoc_list(agent, info)).

:- instance obs(env).
:- instance car_obs(env).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module solutions.

%-----------------------------------------------------------------------------%

:- instance obs(env) where [ ].

:- instance car_obs(env) where [
    (time(env(T, _)) = T),
    (veloc(env(_, Map), B) = veloc(Map^elem(B))),
    (yaw(env(_, Map), B) = yaw(Map^elem(B))),
    (pos(env(_, Map), B) = pos(Map^elem(B))),
    (x_pos(env(_, Map), B) = x(pos(Map^elem(B)))),
    (y_pos(env(_, Map), B) = y(pos(Map^elem(B)))),
    (x_dist(Env, B, C) = x_pos(Env, B) - x_pos(Env, C)),
    (y_dist(Env, B, C) = y_pos(Env, B) - y_pos(Env, C)),
    (veloc_diff(Env, B, C) = veloc(Env, B) - veloc(Env, C)),
    (net_time_gap(Env, B, C) = R :-
        if      V = veloc(Env, B), V \= 0.0
        then    R = x_dist(Env, C, B) / V
        else    false
    ),
    (time_to_collision(Env, B, C) = R :-
        if      VD = veloc_diff(Env, B, C), VD \= 0.0
        then    R = x_dist(Env, C, B) / VD
        else    false
    )
].

%-----------------------------------------------------------------------------%
:- end_module domain.car.obs.env.
%-----------------------------------------------------------------------------%

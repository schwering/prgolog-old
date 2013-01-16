%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: util.vector_space.m.
% Main author: schwering.
%
% Some implementations 
%
%-----------------------------------------------------------------------------%

:- module util.vector_space.impl.

:- interface.

:- instance vector_space(int).
:- instance vector_space(float).
:- instance vector_space({T1, T2})
    <= (vector_space(T1), vector_space(T2)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.

%-----------------------------------------------------------------------------%

:- instance vector_space(int) where [
    func(abs/1) is int.abs,
    func(plus/2) is int.(+),
    func(minus/2) is int.(-),
    (Float `sc_mult` Int = floor_to_int(Float `float.'*'` float(Int))),
    pred(lt/2) is int.(<),
    pred(leq/2) is int.(=<),
    pred(gt/2) is int.(>),
    pred(geq/2) is int.(>=)
].

:- instance vector_space(float) where [
    func(abs/1) is float.abs,
    func(plus/2) is float.(+),
    func(minus/2) is float.(-),
    func(sc_mult/2) is float.(*),
    pred(lt/2) is float.(<),
    pred(leq/2) is float.(=<),
    pred(gt/2) is float.(>),
    pred(geq/2) is float.(>=)
].

:- instance vector_space({T1, T2})
    <= (vector_space(T1), vector_space(T2)) where [
    (abs({X, Y}) = {abs(X), abs(Y)}),
    ({X1, Y1} `plus` {X2, Y2} = {X1 `plus` X2, Y1 `plus` Y2}),
    ({X1, Y1} `minus` {X2, Y2} = {X1 `minus` X2, Y1 `minus` Y2}),
    (Float `sc_mult` {X, Y} = {Float `sc_mult` X, Float `sc_mult` Y}),
    ({X1, Y1} `lt`  {X2, Y2} :- X1 `lt`  X2, Y1 `lt`  Y2),
    ({X1, Y1} `leq` {X2, Y2} :- X1 `leq` X2, Y1 `leq` Y2),
    ({X1, Y1} `gt`  {X2, Y2} :- X1 `gt`  X2, Y1 `gt`  Y2),
    ({X1, Y1} `geq` {X2, Y2} :- X1 `geq` X2, Y1 `geq` Y2)
].

:- instance vector_space({T1, T2, T3})
    <= (vector_space(T1), vector_space(T2), vector_space(T3)) where [
    (abs({X, Y, Z}) = {abs(X), abs(Y), abs(Z)}),
    ({X1, Y1, Z1} `plus` {X2, Y2, Z2} =
        {X1 `plus` X2, Y1 `plus` Y2, Z1 `plus` Z2}),
    ({X1, Y1, Z1} `minus` {X2, Y2, Z2} =
        {X1 `minus` X2, Y1 `minus` Y2, Z1 `minus` Z2}),
    (Float `sc_mult` {X, Y, Z} =
        {Float `sc_mult` X, Float `sc_mult` Y, Float `sc_mult` Z}),
    ({X1, Y1, Z1} `lt`  {X2, Y2, Z2} :- X1 `lt`  X2, Y1 `lt`  Y2, Z1 `lt`  Z2),
    ({X1, Y1, Z1} `leq` {X2, Y2, Z2} :- X1 `leq` X2, Y1 `leq` Y2, Z1 `leq` Z2),
    ({X1, Y1, Z1} `gt`  {X2, Y2, Z2} :- X1 `gt`  X2, Y1 `gt`  Y2, Z1 `gt`  Z2),
    ({X1, Y1, Z1} `geq` {X2, Y2, Z2} :- X1 `geq` X2, Y1 `geq` Y2, Z1 `geq` Z2)
].

%-----------------------------------------------------------------------------%
:- end_module util.vector_space.impl.
%-----------------------------------------------------------------------------%

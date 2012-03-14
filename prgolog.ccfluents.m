% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Helper functions for continuous fluents.
%
% Fluents are represented by higher-order boolean formulas.
% The situation argument is suppressed in test actions and then restored at the
% Golog program's runtime.
%
% As explained in the parent module, the fact that we use boolean formulas
% instead of normal predicates is due to a technicality in Mercury.
%
% Christoph Schwering (schwering@gmail.com)

:- module prgolog.ccfluents.

:- interface.

:- import_module lp.
:- import_module term.
:- import_module varset.

:- type aterm.

:- type ccfunfluent(A) == funfluent(A, aterm).

:- type var_supply == var_supply(generic).

:- func constant(float) = aterm.
:- mode constant(in) = out is det.

:- func variable(var) = aterm.
:- mode variable(in) = out is det.

:- func float * var = aterm.
:- mode in * in = out is det.

:- func aterm + aterm = aterm.
:- mode in + in = out is det.

:- func aterm - aterm = aterm.
:- mode in - in = out is det.

:- func aterm `=<` aterm = equation.
:- mode in `=<` in = out is det.

:- func aterm `>=` aterm = equation.
:- mode in `>=` in = out is det.

:- func aterm `=` aterm = equation.
:- mode in `=` in = out is det.


:- func variables(varset) = list(coeff).
:- mode variables(in) = out is det.


:- implementation.

:- import_module list.
:- import_module pair.

:- type summand ---> mult(float, var) ; const(float).

:- type aterm == list(summand).


constant(C) = [const(C)].


variable(V) = [mult(1.0, V)].


C * V = [mult(C, V)].


T1 + T2 = T1 ++ T2.


T1 - T2 = T1 ++ T3 :-
    T3 = map((func(Summand) = Summand1 :-
        (   Summand = mult(C, V), Summand1 = mult(-C, V)
        ;   Summand = const(C),   Summand1 = const(-C) )
    ), T2).


:- pred split(aterm, list(coeff), float).
:- mode split(in, out, out) is det.

split([], [], 0.0).
split([const(C0) | Ts], Vs, C0 + C1) :- split(Ts, Vs, C1).
split([mult(C, V) | Ts], [(V - C) | Vs], C1) :- split(Ts, Vs, C1).


T1 `=<` T2 = eqn(Sum, (=<), -1.0 * Constant) :- split(T1 - T2, Sum, Constant).


T1 `>=` T2 = eqn(Sum, (>=), -1.0 * Constant) :- split(T1 - T2, Sum, Constant).


T1 `=` T2 = eqn(Sum, (=), -1.0 * Constant) :- split(T1 - T2, Sum, Constant).


variables(VS) = map((func(V) = (V - 1.0)), vars(VS)).


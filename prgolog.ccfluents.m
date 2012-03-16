%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: prgolog.fluents.m.
% Main author: schwering.
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
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module prgolog.ccfluents.

:- interface.

:- import_module lp.
:- import_module map.
:- use_module term.
:- use_module varset.

%-----------------------------------------------------------------------------%

:- type var == term.var.
:- type vargen.

:- func varset(vargen) = varset.varset.
:- mode varset(in) = out is det.

:- func init_vargen = vargen.
:- mode init_vargen = out is det.

:- func new_var(vargen, vargen) = var.
:- mode new_var(in, out) = out is det.

:- func new_variable(vargen, vargen) = aterm.
:- mode new_variable(in, out) = out is det.

:- func null_var = var.
:- mode null_var = out is det.

%-----------------------------------------------------------------------------%

:- type aterm.
:- type tfunc(T) == (func(aterm) = T).
:- type tfunc == tfunc(aterm).
:- type ccfunfluent(A) == funfluent(A, tfunc). % not needed -- remove?

:- func constant(float) = aterm.
:- mode constant(in) = out is det.

:- func variable(var) = aterm.
:- mode variable(in) = out is det.

:- func float * aterm = aterm.
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

:- func variables(vargen) = list(var).
:- mode variables(in) = out is det.

:- func variable_sum(vargen) = list(coeff).
:- mode variable_sum(in) = out is det.

:- func eval(map(var, float), aterm) = float.
:- mode eval(in, in) = out is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- type vargen ---> vargen(varset.varset).

%-----------------------------------------------------------------------------%

varset(vargen(VS)) = VS.

init_vargen = VG :- varset.init(VS), _ = new_var(vargen(VS), VG).

null_var = V :- varset.init(VS), V = new_var(vargen(VS), _).

new_var(vargen(!.VS), vargen(!:VS)) = V :- varset.new_var(V, !VS).

new_variable(VG0, VG1) = variable(new_var(VG0, VG1)).

%-----------------------------------------------------------------------------%

:- type summand ---> mult(float, var) ; const(float).

:- type aterm == list(summand).

%-----------------------------------------------------------------------------%

constant(C) = [const(C)].

variable(V) = [mult(1.0, V)].

%-----------------------------------------------------------------------------%

_ * [] = [].
C0 * [const(C1) | Ts] = [const(C0*C1) | C0 * Ts].
C0 * [mult(C1, V) | Ts] = [mult(C0*C1, V) | C0 * Ts].

T1 + T2 = T1 ++ T2.

T1 - T2 = T1 ++ T3 :-
    T3 = map((func(Summand) = Summand1 :-
        (   Summand = mult(C, V), Summand1 = mult(-C, V)
        ;   Summand = const(C),   Summand1 = const(-C) )
    ), T2).

%-----------------------------------------------------------------------------%

:- pred split(aterm, list(coeff), float).
:- mode split(in, out, out) is det.

split([], [], 0.0).
split([const(C0) | Ts], Vs, C0 + C1) :- split(Ts, Vs, C1).
split([mult(C, V) | Ts], [(V - C) | Vs], C1) :- split(Ts, Vs, C1).

T1 `=<` T2 = eqn(Sum, (=<), -Constant) :- split(T1 - T2, Sum, Constant).
T1 `>=` T2 = eqn(Sum, (>=), -Constant) :- split(T1 - T2, Sum, Constant).
T1 `=`  T2 = eqn(Sum, (=),  -Constant) :- split(T1 - T2, Sum, Constant).

variables(vargen(VS)) = varset.vars(VS).

variable_sum(vargen(VS)) = map((func(V) = (V - 1.0)), varset.vars(VS)).

eval(_, []) = 0.0.
eval(M, [const(C) | Ts]) = C + eval(M, Ts).
eval(M, [mult(C, V) | Ts]) = C * det_elem(V, M) + eval(M, Ts).

%-----------------------------------------------------------------------------%
:- end_module prgolog.ccfluents.
%-----------------------------------------------------------------------------%

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

:- use_module lpq.
:- import_module map.
:- import_module rat.
:- use_module term.
:- use_module varset.

%-----------------------------------------------------------------------------%

:- type var == term.var.
:- type vargen.

:- func varset(vargen) = varset.varset.
:- mode varset(in) = out is det.

:- func init_vargen = vargen.
:- mode init_vargen = out is det.

:- pred new_var(vargen, vargen, var).
:- mode new_var(in, out, out) is det.

:- func new_variable(vargen, vargen) = aterm.
:- mode new_variable(in, out) = out is det.

:- func null_var = var.
:- mode null_var = out is det.

%-----------------------------------------------------------------------------%

:- type number == rat.
:- type constraint == lpq.constraint.
:- type aterm.
:- type tfunc(T) == ( func(aterm) = T ).
:- type tfunc == tfunc(aterm).
:- type ccfunfluent(A) == funfluent(A, tfunc). % not needed -- remove?

:- func constant(number) = aterm.
:- mode constant(in) = out is det.

:- func variable(var) = aterm.
:- mode variable(in) = out is det.

:- func number * aterm = aterm.
:- mode in * in = out is det.

:- func aterm + aterm = aterm.
:- mode in + in = out is det.

:- func aterm - aterm = aterm.
:- mode in - in = out is det.

:- func aterm `=<` aterm = constraint.
:- mode in `=<` in = out is det.

:- func aterm `>=` aterm = constraint.
:- mode in `>=` in = out is det.

:- func aterm `=` aterm = constraint.
:- mode in `=` in = out is det.

%-----------------------------------------------------------------------------%

:- type objective ---> min(aterm) ; max(aterm).

:- pred solve(vargen, list(constraint)).
:- mode solve(in, in) is semidet.

:- pred solve(vargen, list(constraint), objective, map(var, number), number).
:- mode solve(in, in, in, out, out) is semidet.

:- func variables(vargen) = list(var).
:- mode variables(in) = out is det.

:- func variable_sum(vargen) = aterm.
:- mode variable_sum(in) = out is det.

:- func eval(map(var, number), aterm) = number.
:- mode eval(in, in) = out is det.

:- func eval_float(map(var, number), aterm) = float.
:- mode eval_float(in, in) = out is det.

%-----------------------------------------------------------------------------%

:- type time == aterm.
:- type ccformula(A) == ( func(time, sit(A)) = list(constraint) ).

:- func (ccformula(A) and ccformula(A)) = ccformula(A).
:- mode (in and in) = out is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- type vargen ---> vargen(varset.varset).

%-----------------------------------------------------------------------------%

varset(vargen(VS)) = VS.

init_vargen = VG :- varset.init(VS), new_var(vargen(VS), VG, _).

null_var = V :- varset.init(VS), new_var(vargen(VS), _, V).

new_var(vargen(!.VS), vargen(!:VS), V) :- varset.new_var(V, !VS).

new_variable(VG0, VG1) = variable(V) :- new_var(VG0, VG1, V).

%-----------------------------------------------------------------------------%

:- type summand ---> mult(number, var) ; const(number).

:- type aterm == list(summand).

%-----------------------------------------------------------------------------%

constant(C) = [const(C)].

variable(V) = [mult(one, V)].

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

:- pred split(aterm, lpq.lp_terms, number).
:- mode split(in, out, out) is det.

split([], [], zero).
split([const(C0) | Ts], Vs, C0 + C1) :- split(Ts, Vs, C1).
split([mult(C, V) | Ts], [(V - C) | Vs], C1) :- split(Ts, Vs, C1).

T1 `=<` T2 = lpq.construct_constraint(Sum, lpq.lp_lt_eq, -Constant) :-
    split(T1 - T2, Sum, Constant).
T1 `=`  T2 = lpq.construct_constraint(Sum, lpq.lp_eq,    -Constant) :-
    split(T1 - T2, Sum, Constant).
T1 `>=` T2 = lpq.construct_constraint(Sum, lpq.lp_gt_eq, -Constant) :-
    split(T1 - T2, Sum, Constant).

%-----------------------------------------------------------------------------%

solve(Vargen, Constraints) :-
    solve(Vargen, Constraints, max([]), _, _).

%:- pragma memo(solve/5, [allow_reset, fast_loose]). 

solve(Vargen, Constraints, Obj, Map, Val) :-
    (   Obj = max(ObjTerm), Dir = lpq.max
    ;   Obj = min(ObjTerm), Dir = lpq.min
    ),
    split(ObjTerm, ObjVars, ObjCons),
    Res = lpq.solve(Constraints, Dir, ObjVars, varset(Vargen)),
    Res = lpq.lp_res_satisfiable(Val0, Map),
    Val = Val0 + ObjCons.

variables(vargen(VS)) = varset.vars(VS).

variable_sum(vargen(VS)) = map((func(V) = mult(one, V)), varset.vars(VS)).

eval(_, []) = zero.
eval(M, [const(C) | Ts]) = C + eval(M, Ts).
eval(M, [mult(C, V) | Ts]) = C * det_elem(V, M) + eval(M, Ts).

eval_float(M, T) = float(numer(R)) / float(denom(R)) :- R = eval(M, T).

%-----------------------------------------------------------------------------%

(F1 and F2) = ( func(T, S) = F1(T, S) ++ F2(T, S) ).

%-----------------------------------------------------------------------------%
:- end_module prgolog.ccfluents.
%-----------------------------------------------------------------------------%

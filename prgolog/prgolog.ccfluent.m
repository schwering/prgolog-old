%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: prgolog.ccfluent.m.
% Main author: schwering.
%
% Utilities for continuous fluents including constraint solving capabilities.
%
% Currently we use the COIN-OR CLP solver via the OSI interface.  The module
% osi.m provides a Mercury (very small subset only for our purposes) binding to
% the C++ API.
%
% It's generally easy to use the modules lp.m and/or lp_rational.m from the
% Mercury compiler (not library!) instead.  However, they are either
% numerically pretty unstable or buggy or I used them totally wrongly. 
%
% The preprocessing is pretty much aligend to OSI: coefficients are aggregated
% so that no variable occurs twice in a constraint and trivial constraints
% (left hand side has no positive coefficients) are handled so that only
% variables with non-zero coefficient are given to the solver.
%
% The variable-to-solution-map is stored in an assoc_list.  Variables with
% value 0.0 are omitted in the list!
%
%-----------------------------------------------------------------------------%

:- module prgolog.ccfluent.

:- interface.

:- import_module assoc_list.
:- use_module osi.
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

%:- mode null_var = out is det.

%-----------------------------------------------------------------------------%

:- type number == float.
:- type constraint == osi.constraint.
:- type aterm.
:- type tfunc(T) == ( func(aterm) = T ).
:- type tfunc == tfunc(aterm).
:- type ccfunfluent(A) == funfluent(A, tfunc). % not needed -- remove?

:- func constant(number) = aterm.
:- mode constant(in) = out is det.

:- func variable(var) = aterm.
:- mode variable(in) = out is det.

:- func - aterm = aterm.
:- func + aterm = aterm.

:- func number * aterm = aterm.
:- func aterm + aterm = aterm.
:- func aterm - aterm = aterm.

:- func (aterm =< aterm) = constraint.
:- func (aterm >= aterm) = constraint.
:- func (aterm =  aterm) = constraint.

:- pred holds_trivially(constraint::in) is semidet.

%-----------------------------------------------------------------------------%

:- type objective ---> min(aterm) ; max(aterm).

:- pred solve(vargen, list(constraint)).
:- mode solve(in, in) is semidet.

:- pred solve(vargen, list(constraint), assoc_list(var, number), number).
:- mode solve(in, in, out, out) is semidet.

:- pred solve(vargen, list(constraint), objective,
              assoc_list(var, number), number).
:- mode solve(in, in, in, out, out) is semidet.

:- func variables(vargen) = list(var).
:- mode variables(in) = out is det.

:- func variable_sum(vargen) = aterm.
:- mode variable_sum(in) = out is det.

:- func eval(assoc_list(var, number), aterm) = number.
:- mode eval(in, in) = out is det.

:- func eval_float(assoc_list(var, number), aterm) = float.
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

init_vargen = vargen(VS) :- varset.init(VS).%, new_var(vargen(VS), VG, _).

%null_var = V :- varset.init(VS), new_var(vargen(VS), _, V).

new_var(vargen(!.VS), vargen(!:VS), V) :- varset.new_var(V, !VS).

new_variable(VG0, VG1) = variable(V) :- new_var(VG0, VG1, V).

%-----------------------------------------------------------------------------%

:- type summand ---> mult(number, var) ; const(number).

:- type aterm == list(summand).

%-----------------------------------------------------------------------------%

constant(C) = [const(C)].

variable(V) = [mult(1.0, V)].

%-----------------------------------------------------------------------------%

_ * [] = [].
C0 * [const(C1) | Ts] = [const(C0*C1) | C0 * Ts].
C0 * [mult(C1, V) | Ts] = [mult(C0*C1, V) | C0 * Ts].

+ T = T.

- T = map((func(Summand) = Summand1 :-
        (   Summand = const(C),   Summand1 = const(-1.0 * C)
        ;   Summand = mult(C, V), Summand1 = mult(-1.0 * C, V))
    ), T).

T1 + T2 = T1 ++ T2.

T1 - T2 = T1 ++ T3 :-
    T3 = map((func(Summand) = Summand1 :-
        (   Summand = mult(C, V), Summand1 = mult(-C, V)
        ;   Summand = const(C),   Summand1 = const(-C) )
    ), T2).


:- pred split(aterm, list(pair(var, number)), number).
:- mode split(in, out, out) is det.

split([], [], 0.0).
split([const(C0) | Ts], Vs, C0 + C1) :- split(Ts, Vs, C1).
split([mult(C, V) | Ts], [(V - C) | Vs], C1) :- split(Ts, Vs, C1).


:- pred aggregate(list(pair(var, number))::in,
                  list(pair(var, number))::out) is det.

aggregate(L, aggregate_2(sort(L))).


:- func aggregate_2(list(pair(var, number))::in) =
                   (list(pair(var, number))::out) is det.

aggregate_2([]) = [].
aggregate_2([C @ (V - A) | Cs]) =
    (   if      Cs = [(V - A0) | Cs0]       % aggregate coeffs of dupe vars
        then    aggregate_2([(V - (A+A0)) | Cs0])
        else if A \= 0.0                    % remove vars with coeff 0
        then    [C | aggregate_2(Cs)]
        else    aggregate_2(Cs)
    ).


(T1 =< T2) = osi.construct_constraint(Sum1, osi.(=<), -Constant) :-
    split(T1 - T2, Sum0, Constant), aggregate(Sum0, Sum1).
(T1 =  T2) = osi.construct_constraint(Sum1, osi.(=),  -Constant) :-
    split(T1 - T2, Sum0, Constant), aggregate(Sum0, Sum1).
(T1 >= T2) = osi.construct_constraint(Sum1, osi.(>=), -Constant) :-
    split(T1 - T2, Sum0, Constant), aggregate(Sum0, Sum1).


holds_trivially(C) :- osi.holds_trivially(C).

%-----------------------------------------------------------------------------%

solve(Vargen, Constraints) :-
    solve(Vargen, Constraints, min([]), _, _).

solve(Vargen, Constraints, Map, Val) :-
    solve(Vargen, Constraints, min([]), Map, Val).

%:- pragma memo(solve/5, [allow_reset, fast_loose]). 

:- import_module io.
solve(Vargen, Constraints, Obj, Map, Val) :-
    (   Obj = max(ObjTerm), Dir = osi.max
    ;   Obj = min(ObjTerm), Dir = osi.min
    ),
    split(ObjTerm, ObjVars, ObjCons),
    Res = osi.solve(Constraints, Dir, ObjVars, varset(Vargen)),
    Res = osi.satisfiable(Val0, Map),
    Val = Val0 + ObjCons.

variables(vargen(VS)) = varset.vars(VS).

variable_sum(vargen(VS)) = map((func(V) = mult(1.0, V)), varset.vars(VS)).

eval(_, []) = 0.0.
eval(M, [const(C) | Ts]) = C + eval(M, Ts).
eval(M, [mult(C, V) | Ts]) = C * F + eval(M, Ts) :-
    (   if      F0 = elem(V, M)
        then    F = F0
        else    F = 0.0
    ).

%eval_float(M, T) = float(numer(R)) / float(denom(R)) :- R = eval(M, T).
eval_float(M, T) = eval(M, T).

%-----------------------------------------------------------------------------%

(F1 and F2) = ( func(T, S) = F1(T, S) ++ F2(T, S) ).

%-----------------------------------------------------------------------------%
:- end_module prgolog.ccfluent.
%-----------------------------------------------------------------------------%

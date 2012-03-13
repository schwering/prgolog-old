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

:- type ccfunfluent(A) == funfluent(A, list(coeff)).

:- func float * term.var = list(coeff).
:- mode in * in = out is det.

:- func list(coeff) + list(coeff) = list(coeff).
:- mode in + in = out is det.

:- func list(coeff) - list(coeff) = list(coeff).
:- mode in - in = out is det.

:- func list(coeff) `=<` float = equation.
:- mode in `=<` in = out is det.

:- func list(coeff) `>=` float = equation.
:- mode in `>=` in = out is det.

:- func list(coeff) `=` float = equation.
:- mode in `=` in = out is det.


:- implementation.

:- import_module pair.

Constant * Variable = [Variable - Constant].

Coeff1 + Coeff2 = Coeff1 ++ Coeff2.

Coeff1 - Coeff2 = Coeff1 ++ Coeff3 :-
    Coeff3 = map((func(V - C) = V - (-C)), Coeff2).

Sum `=<` Constant = eqn(Sum, (=<), Constant).

Sum `>=` Constant = eqn(Sum, (>=), Constant).

Sum `=` Constant = eqn(Sum, (=), Constant).


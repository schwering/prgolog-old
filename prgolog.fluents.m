% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Helper functions for test actions.
%
% Fluents are represented by higher-order boolean formulas.
% The situation argument is suppressed in test actions and then restored at the
% Golog program's runtime.
%
% As explained in the parent module, the fact that we use boolean formulas
% instead of normal predicates is due to a technicality in Mercury.
%
% Christoph Schwering (schwering@gmail.com)

:- module prgolog.fluents.

:- interface.

:- func f(R) = funfluent(A, R).
:- mode f(in) = out is det.

:- func r((pred)) = relfluent(A).
:- mode r(in((pred) is semidet)) = out is det.

:- func ==(funfluent(A, R), funfluent(A, R)) `with_type` relfluent(A).
:- func and(relfluent(A), relfluent(A)) `with_type` relfluent(A).
:- func or(relfluent(A), relfluent(A)) `with_type` relfluent(A).
:- func neg(relfluent(A)) `with_type` relfluent(A).


:- implementation.

f(R) = (func(_) = R).
r(P) = (func(_) = ( if call(P) then bool.yes else bool.no )).

==(Lhs, Rhs, S) = ( if Lhs(S) = Rhs(S) then bool.yes else bool.no ).
and(T1, T2, S) = bool.and(T1(S), T2(S)).
or(T1, T2, S) = bool.or(T1(S), T2(S)).
neg(T, S) = bool.not(T(S)).


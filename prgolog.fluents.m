% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Helper functions for test actions.
%
% As noted in the prgolog module, fluent expressions in test actions are not
% deconstructed and evaluated by a holds/2 predicate as in usual Prolog
% implementations.
% Instead, each test action has an associated unary higher-order predicate
% which is simply called with the current situation term as single argument.
%
% This module provides some predicates to combine relational and functional
% fluents.
%
% Christoph Schwering (schwering@gmail.com)

:- module prgolog.fluents.

:- interface.

:- func f(R) = funfluent(A, R).
:- mode f(in) = out is det.

:- func r((pred)) = relfluent(A).
:- mode r(in((pred) is semidet)) = out is det.

:- pred ==(funfluent(A, R), funfluent(A, R), sit(A)).
:- mode ==(in(funfluent), in(funfluent), in) is semidet.

:- pred and(relfluent(A), relfluent(A), sit(A)).
:- mode and(in(relfluent), in(relfluent), in) is semidet.

:- pred or(relfluent(A), relfluent(A), sit(A)).
:- mode or(in(relfluent), in(relfluent), in) is semidet.

:- pred neg(relfluent(A), sit(A)).
:- mode neg(in(relfluent), in) is semidet.


:- implementation.

f(R) = ((func(_) = R) is det).
r(P) = (pred(_::in) is semidet :- call(P)).

==(Lhs, Rhs, S) :- Lhs(S) = Rhs(S).
and(T1, T2, S) :- T1(S), T2(S).
or(T1, T2, S) :- T1(S) ; T2(S).
neg(T, S) :- not T(S).


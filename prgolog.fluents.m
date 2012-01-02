% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0

:- module prgolog.fluents.

:- interface.

:- pred eq(funfluent(A, R), funfluent(A, R), sit(A)).
:- mode eq(in(funfluent), in(funfluent), in) is semidet.

:- pred eqv(funfluent(A, R), R, sit(A)).
:- mode eqv(in(funfluent), in, in) is semidet.

:- pred veq(R, funfluent(A, R), sit(A)).
:- mode veq(in, in(funfluent), in) is semidet.

:- pred and(relfluent(A), relfluent(A), sit(A)).
:- mode and(in(relfluent), in(relfluent), in) is semidet.

:- pred or(relfluent(A), relfluent(A), sit(A)).
:- mode or(in(relfluent), in(relfluent), in) is semidet.

:- pred neg(relfluent(A), sit(A)).
:- mode neg(in(relfluent), in) is semidet.


:- implementation.

eq(Lhs, Rhs, S) :- Lhs(S) = Rhs(S).
eqv(Lhs, Rhs, S) :- Lhs(S) = Rhs.
veq(Lhs, Rhs, S) :- Lhs = Rhs(S).
and(T1, T2, S) :- T1(S), T2(S).
or(T1, T2, S) :- T1(S) ; T2(S).
neg(T, S) :- not T(S).


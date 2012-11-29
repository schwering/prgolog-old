%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: arithmetic.m.
% Main author: schwering.
%
% Typeclass for arithmetic operations.
%
%-----------------------------------------------------------------------------%

:- module arithmetic.

:- interface.

:- use_module prgolog.
:- use_module prgolog.ccfluent.
:- use_module rat.
:- use_module rational.

:- typeclass arithmetic(N) where [
    func zero = N,
    func one = N,
    func abs(N) = N,
    func + N = N,
    func - N = N,
    func N + N = N,
    func N - N = N,
    func N * N = N,
    func N / N = N,
    func unchecked_quotient(N, N) = N,
    func epsilon = N,
    func from_float(float) = N,
    func to_float(N) = float,
    pred (N::in) < (N::in) is semidet,
    pred (N::in) > (N::in) is semidet,
    pred (N::in) =< (N::in) is semidet,
    pred (N::in) >= (N::in) is semidet
].

:- instance arithmetic(int).
:- instance arithmetic(float).
:- instance arithmetic(rat.rat).
:- instance arithmetic(rational.rational).
%:- instance arithmetic(prgolog.ccfluent.aterm).

%-----------------------------------------------------------------------------%

:- func two = N <= arithmetic(N).
:- func three = N <= arithmetic(N).
:- func four = N <= arithmetic(N).

:- func pow(N, int) = N <= arithmetic(N).

%-----------------------------------------------------------------------------%

:- pred bin_search(func(X) = Y, X, X, Y, X) <= (arithmetic(X), arithmetic(Y)).
:- mode bin_search(in(func(in) = out is det), in, in, in, out) is semidet.
:- mode bin_search(in(func(in) = out is semidet), in, in, in, out) is semidet.

%-----------------------------------------------------------------------------%

:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- use_module float.
:- use_module int.
:- use_module integer.

%-----------------------------------------------------------------------------%

two = one + one.
three = two + one.
four = pow(two, 2).

pow(X, N) = (    if N = 0 then one
            else if N > 0 then X * pow(X, N - 1)
            else               one / pow(X, -N) ).

%-----------------------------------------------------------------------------%

:- pred in_eps_ball(N::in, N::in) is semidet <= arithmetic(N).

in_eps_ball(X, Y) :- abs(X - Y) =< epsilon.


:- pred in_2eps_ball(func(X) = Y, X, Y) <= (arithmetic(X), arithmetic(Y)).
:- mode in_2eps_ball(in(func(in) = out is det), in, in) is semidet.
:- mode in_2eps_ball(in(func(in) = out is semidet), in, in) is semidet.

in_2eps_ball(F, X, Y) :-
    F(X - two * epsilon) =< Y,
    F(X + two * epsilon) >= Y.

%-----------------------------------------------------------------------------%

bin_search(F, XMin, XMax, YGoal, X) :-
    if F(XMin) > F(XMax) then
        bin_search_2(func(X1) = -F(X1) is semidet, XMin, XMax, -YGoal, X)
    else
        bin_search_2(F, XMin, XMax, YGoal, X).


:- pred bin_search_2(func(X) = Y, X, X, Y, X) <= (arithmetic(X), arithmetic(Y)).
:- mode bin_search_2(in(func(in) = out is det), in, in, in, out) is semidet.
:- mode bin_search_2(in(func(in) = out is semidet), in, in, in, out) is semidet.

bin_search_2(F, XMin, XMax, YGoal, X) :-
    XMid = (XMin + XMax) / two,
%    trace [io(!IO)] (
%        write_string("binary_search_2(", !IO),
%        write(XMin, !IO),
%        write_string(", ", !IO),
%        write(XMax, !IO),
%        write_string(", ", !IO),
%        write(YGoal, !IO),
%        write_string(")    ", !IO),
%        write(XMid, !IO),
%        nl(!IO),
%        write_string("eps = ", !IO), write(float.epsilon, !IO), nl(!IO),
%        ( if FXMin = F(XMin), FXMax = F(XMax), FXMid = F(XMid) then
%            write_string("Xmin = ", !IO), write(XMin, !IO), nl(!IO),
%            write_string("Xmid = ", !IO), write(XMid, !IO), nl(!IO),
%            write_string("Xmax = ", !IO), write(XMax, !IO), nl(!IO),
%            write_string("f(Xmin-) = ", !IO), write(FXMin - epsilon, !IO), nl(!IO),
%            write_string("f(Xmin)  = ", !IO), write(FXMin, !IO), nl(!IO),
%            write_string("f(Xmid)  = ", !IO), write(FXMid, !IO), nl(!IO),
%            write_string("ygoal    = ", !IO), write(YGoal, !IO), nl(!IO),
%            write_string("f(Xmax)  = ", !IO), write(FXMax, !IO), nl(!IO),
%            write_string("f(Xmax+) = ", !IO), write(FXMax + epsilon, !IO), nl(!IO),
%            write_string("2eps = ", !IO), write(2.0 `float.'*'` float.epsilon, !IO), nl(!IO),
%            write_string("diff = ", !IO), write(XMax - XMin, !IO), nl(!IO),
%            write_string("minu = ", !IO), write(XMax - XMin - two * epsilon, !IO), nl(!IO),
%            write_string("f-diff = ", !IO), write(FXMax - FXMin, !IO), nl(!IO)
%        else true ),
%        true
%    ),
    (        if XMax - XMin =< two * epsilon,
                F(XMid - two * epsilon) =< F(XMin),
                F(XMid + two * epsilon) >= F(XMax) then X = XMid
        else if XMin >= XMax    then fail
        else if F(XMid) < YGoal then bin_search_2(F, XMid, XMax, YGoal, X)
        else /* F(XMid) > YGoal */   bin_search_2(F, XMin, XMid, YGoal, X)
    ).

%-----------------------------------------------------------------------------%

:- instance arithmetic(int) where [
    zero = 0,
    one = 1,
    func(abs/1) is int.abs,
    + X = int.'+'(X),
    - X = int.'-'(X),
    X + Y = int.'+'(X, Y),
    X - Y = int.'-'(X, Y),
    X * Y = int.'*'(X, Y),
    X / Y = int.'/'(X, Y),
    unchecked_quotient(X, Y) = int.unchecked_quotient(X, Y),
    epsilon = 0,
    from_float(F) = float.floor_to_int(F),
    to_float(I) = float.float(I),
    X < Y :- int.'<'(X, Y),
    X > Y :- int.'>'(X, Y),
    X =< Y :- int.'=<'(X, Y),
    X >= Y :- int.'>='(X, Y)
].

:- instance arithmetic(float) where [
    zero = 0.0,
    one = 1.0,
    func(abs/1) is float.abs,
    + X = float.'+'(X),
    - X = float.'-'(X),
    X + Y = float.'+'(X, Y),
    X - Y = float.'-'(X, Y),
    X * Y = float.'*'(X, Y),
    X / Y = float.'/'(X, Y),
    unchecked_quotient(X, Y) = float.unchecked_quotient(X, Y),
    epsilon = float.epsilon,
    from_float(F) = F,
    to_float(F) = F,
    X < Y :- float.'<'(X, Y),
    X > Y :- float.'>'(X, Y),
    X =< Y :- float.'=<'(X, Y),
    X >= Y :- float.'>='(X, Y)
].


:- instance arithmetic(rat.rat) where [
    zero = rat.zero,
    one = rat.one,
    func(abs/1) is rat.abs,
    + X = rat.'+'(X),
    - X = rat.'-'(X),
    X + Y = rat.'+'(X, Y),
    X - Y = rat.'-'(X, Y),
    X * Y = rat.'*'(X, Y),
    X / Y = rat.'/'(X, Y),
    unchecked_quotient(X, Y) = rat.'/'(X, Y),
    epsilon = rat.rat(1, 52),
    from_float(F) = float_to_rat(F),
    to_float(F) = float.float(rat.numer(F)) / float.float(rat.denom(F)),
    X < Y :- rat.'<'(X, Y),
    X > Y :- rat.'>'(X, Y),
    X =< Y :- rat.'=<'(X, Y),
    X >= Y :- rat.'>='(X, Y)
].


:- instance arithmetic(rational.rational) where [
    zero = rational.zero,
    one = rational.one,
    func(abs/1) is rational.abs,
    + X = rational.'+'(X),
    - X = rational.'-'(X),
    X + Y = rational.'+'(X, Y),
    X - Y = rational.'-'(X, Y),
    X * Y = rational.'*'(X, Y),
    X / Y = rational.'/'(X, Y),
    unchecked_quotient(X, Y) = rational.'/'(X, Y),
    epsilon = rational.rational(1, 52),
    from_float(F) = float_to_rational(F),
    to_float(F) = integer.float(rational.numer(F)) /
                  integer.float(rational.denom(F)),
    X < Y :- rational.'<'(X, Y),
    X > Y :- rational.'>'(X, Y),
    X =< Y :- rational.'=<'(X, Y),
    X >= Y :- rational.'>='(X, Y)
].


:- func float_to_rat(float) = rat.rat.

float_to_rat(Float) = rat.rat(Numer, Denom) :-
    % The reason for maximal denominator 52 is as follows:
    % The first sixteen primes are 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53.
    % Their product is greater than 2^63 (and also greater than 2^64).
    % The product of the first fifteen primes, however, is less than 2^63.
    % Since any product of numbers less than or equal to 52 can be factorized
    % with the first fifteen prime numbers, a maximal denominator of 52 avoids
    % integer overflows.
    float_numer_denom(52, Float, Numer, Denom).


:- func float_to_rational(float) = rational.rational.

float_to_rational(Float) = rational.rational(Numer, Denom) :-
    float_numer_denom(100000, Float, Numer, Denom).


:- pred float_numer_denom(int::in, float::in, int::out, int::out) is det.

:- pragma foreign_proc("C",
    float_numer_denom(Maxden::in, Float::in, Numer::out, Denom::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /*
     * find rational approximation to given real number
     * David Eppstein / UC Irvine / 8 Aug 1993
     * 
     * With corrections from Arno Formella, May 2008
     *
     * x is real number to approx
     * Maxden is the maximum denominator allowed
     *
     * based on the theory of continued fractions
     * if x = a1 + 1/(a2 + 1/(a3 + 1/(a4 + ...)))
     * then best approximation is found by truncating this series
     * (with some adjustments in the last term).
     *
     * Note the fraction can be recovered as the first column of the matrix
     *  ( a1 1 ) ( a2 1 ) ( a3 1 ) ...
     *  ( 1  0 ) ( 1  0 ) ( 1  0 )
     * Instead of keeping the sequence of continued fraction terms,
     * we just keep the last partial product of these matrices.
     */

    MR_Integer m[2][2];
    MR_Float x;
    MR_Integer ai;

    x = Float;

    /* initialize matrix */
    m[0][0] = m[1][1] = 1;
    m[0][1] = m[1][0] = 0;

    /* loop finding terms until denom gets too big */
    while (m[1][0] * (ai = (MR_Integer) x) + m[1][1] <= Maxden) {
        MR_Integer t;
        t = m[0][0] * ai + m[0][1];
        m[0][1] = m[0][0];
        m[0][0] = t;
        t = m[1][0] * ai + m[1][1];
        m[1][1] = m[1][0];
        m[1][0] = t;
        if (x == (MR_Float) ai)
            break; // AF: division by zero
        x = 1.0 / (x - (MR_Float) ai);
        if (x > (MR_Float) 0x7FFFFFFF)
            break;  // AF: representation failure
    }

    Numer = m[0][0];
    Denom = m[1][0];
").

/*
:- instance arithmetic(prgolog.ccfluent.aterm) where [
    zero = prgolog.ccfluent.constant(0.0),
    one = prgolog.ccfluent.constant(1.0),
    abs(T) = T,
    + X = prgolog.ccfluent.'+'(X),
    - X = prgolog.ccfluent.'-'(X),
    X + Y = prgolog.ccfluent.'+'(X, Y),
    X - Y = prgolog.ccfluent.'-'(X, Y),
    X * Y = prgolog.ccfluent.'*'(X, Y),
    X / Y = prgolog.ccfluent.'/'(X, Y),
    unchecked_quotient(X, Y) = prgolog.ccfluent.'/'(X, Y),
    epsilon = zero,
    from_float(F) = prgolog.ccfluent.constant(F),
    to_float(F) = integer.float(rational.numer(F)) /
                  integer.float(rational.denom(F)),
    X < Y :- prgolog.ccfluent.'<'(X, Y),
    X > Y :- prgolog.ccfluent.'>'(X, Y),
    X =< Y :- prgolog.ccfluent.'=<'(X, Y),
    X >= Y :- prgolog.ccfluent.'>='(X, Y)
].
*/

%-----------------------------------------------------------------------------%
:- end_module arithmetic.
%-----------------------------------------------------------------------------%

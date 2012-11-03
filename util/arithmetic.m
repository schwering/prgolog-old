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
    func from_float(float) = N,
    pred (N::in) < (N::in) is semidet,
    pred (N::in) > (N::in) is semidet,
    pred (N::in) =< (N::in) is semidet,
    pred (N::in) >= (N::in) is semidet
].

:- instance arithmetic(float).
:- instance arithmetic(rational.rational).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- use_module float.

%-----------------------------------------------------------------------------%

:- instance arithmetic(float) where [
    zero = 0.0,
    one = 1.0,
    func(abs/1) is float.abs,
    + X = float.'+'(X),
    - X = float.'-'(X),
    X + Y = float.'-'(X, Y),
    X - Y = float.'-'(X, Y),
    X * Y = float.'*'(X, Y),
    X / Y = float.'/'(X, Y),
    from_float(F) = F,
    X < Y :- float.'<'(X, Y),
    X > Y :- float.'>'(X, Y),
    X =< Y :- float.'=<'(X, Y),
    X >= Y :- float.'>='(X, Y)
].

:- instance arithmetic(rational.rational) where [
    zero = rational.zero,
    one = rational.one,
    func(abs/1) is rational.abs,
    + X = rational.'+'(X),
    - X = rational.'-'(X),
    X + Y = rational.'-'(X, Y),
    X - Y = rational.'-'(X, Y),
    X * Y = rational.'*'(X, Y),
    X / Y = rational.'/'(X, Y),
    from_float(F) = rational.rational(N, D) :- float_numer_denom(F, N, D),
    X < Y :- rational.'<'(X, Y),
    X > Y :- rational.'>'(X, Y),
    X =< Y :- rational.'=<'(X, Y),
    X >= Y :- rational.'>='(X, Y)
].

:- pred float_numer_denom(float::in, int::out, int::out) is det.

:- pragma foreign_proc("C",
    float_numer_denom(Float::in, Numer::out, Denom::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /*
     * ** find rational approximation to given real number
     * ** David Eppstein / UC Irvine / 8 Aug 1993
     * **
     * ** With corrections from Arno Formella, May 2008
     * **
     * ** x is real number to approx
     * ** maxden is the maximum denominator allowed
     * **
     * ** based on the theory of continued fractions
     * ** if x = a1 + 1/(a2 + 1/(a3 + 1/(a4 + ...)))
     * ** then best approximation is found by truncating this series
     * ** (with some adjustments in the last term).
     * **
     * ** Note the fraction can be recovered as the first column of the matrix
     * **  ( a1 1 ) ( a2 1 ) ( a3 1 ) ...
     * **  ( 1  0 ) ( 1  0 ) ( 1  0 )
     * ** Instead of keeping the sequence of continued fraction terms,
     * ** we just keep the last partial product of these matrices.
     * */

    MR_Integer m[2][2];
    MR_Float x;
    MR_Integer maxden;
    MR_Integer ai;

    x = Float;
    maxden = 100000;

    /* initialize matrix */
    m[0][0] = m[1][1] = 1;
    m[0][1] = m[1][0] = 0;

    /* loop finding terms until denom gets too big */
    while (m[1][0] * (ai = (MR_Integer) x) + m[1][1] <= maxden) {
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

%-----------------------------------------------------------------------------%
:- end_module arithmetic.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: inf_arithmetic.m.
% Main author: schwering.
%
% Extension of numbers with positive and negative infinity.
% This package adds infinity to instances of the arithmetic typeclass.
%
% However, it does not implement the arithmetic typeclass itself, because
% its functions are all semidet.
%
%-----------------------------------------------------------------------------%

:- module inf_arithmetic.

:- interface.

%-----------------------------------------------------------------------------%

:- use_module arithmetic.

%-----------------------------------------------------------------------------%

:- type num(N) ---> neg_inf ; num(N) ; pos_inf.
:- inst basic_num ---> num(ground).
:- inst infinity ---> neg_inf ; pos_inf.
:- inst neg_inf ---> neg_inf.
:- inst pos_inf ---> pos_inf.
:- inst tuple(X, Y) ---> {X, Y}.

%-----------------------------------------------------------------------------%

:- func number(N)  = num(N) <= arithmetic.arithmetic(N).
:- mode number(in) = out(basic_num) is det.

:- func number_from_float(float) = num(N) <= arithmetic.arithmetic(N).
:- mode number_from_float(in)    = out(basic_num) is det.

:- func number_from_int(int) = num(N) <= arithmetic.arithmetic(N).
:- mode number_from_int(in)  = out(basic_num) is det.

:- func basic(num(N)) = N.
:- mode basic(in(infinity)) = out  is failure.
:- mode basic(in(neg_inf)) = out   is failure.
:- mode basic(in(basic_num)) = out is det.
:- mode basic(in(pos_inf)) = out   is failure.
:- mode basic(in) = out            is semidet.

:- pred basic(num(N)).
:- mode basic(in(infinity))  is failure.
:- mode basic(in(neg_inf))   is failure.
:- mode basic(in(basic_num)) is det.
:- mode basic(in(pos_inf))   is failure.

:- func det_basic(num(N)) = N.
:- mode det_basic(in) = out is det.

:- func negative_infinity = num(N) <= arithmetic.arithmetic(N).
:- mode negative_infinity = out(infinity) is det.

:- func positive_infinity = num(N) <= arithmetic.arithmetic(N).
:- mode positive_infinity = out(infinity) is det.

:- pred negative_infinity(num(N)).
:- mode negative_infinity(in(infinity))  is semidet.
:- mode negative_infinity(in(basic_num)) is failure.
:- mode negative_infinity(in)            is semidet.

:- pred positive_infinity(num(N)).
:- mode positive_infinity(in(infinity))  is semidet.
:- mode positive_infinity(in(basic_num)) is failure.
:- mode positive_infinity(in)            is semidet.

:- pred infinity(num(N)).
:- mode infinity(in(infinity))  is det.
:- mode infinity(in(neg_inf))   is det.
:- mode infinity(in(pos_inf))   is det.
:- mode infinity(in(basic_num)) is failure.
:- mode infinity(in)            is semidet.

:- pred negative(num(N)) <= arithmetic.arithmetic(N).
:- mode negative(in(neg_inf))   is det.
:- mode negative(in(basic_num)) is semidet.
:- mode negative(in(pos_inf))   is failure.
:- mode negative(in)            is semidet.

:- pred positive(num(N)) <= arithmetic.arithmetic(N).
:- mode positive(in(neg_inf))   is failure.
:- mode positive(in(basic_num)) is semidet.
:- mode positive(in(pos_inf))   is det.
:- mode positive(in)            is semidet.

:- func zero = num(N) <= arithmetic.arithmetic(N).
:- mode zero = out(basic_num) is det.

:- func one = num(N) <= arithmetic.arithmetic(N).
:- mode one = out(basic_num) is det.

:- func abs(num(N))        = num(N) <= arithmetic.arithmetic(N).
:- mode abs(in(neg_inf))   = out(pos_inf)   is det.
:- mode abs(in(pos_inf))   = out(pos_inf)   is det.
:- mode abs(in(basic_num)) = out(basic_num) is det.
:- mode abs(in)            = out            is det.

:- func + num(N)        = num(N) <= arithmetic.arithmetic(N).
:- mode + in(neg_inf)   = out(neg_inf)   is det.
:- mode + in(basic_num) = out(basic_num) is det.
:- mode + in(pos_inf)   = out(pos_inf)   is det.
:- mode + in            = out            is det.

:- func - num(N)        = num(N) <= arithmetic.arithmetic(N).
:- mode - in(neg_inf)   = out(pos_inf)   is det.
:- mode - in(basic_num) = out(basic_num) is det.
:- mode - in(pos_inf)   = out(neg_inf)   is det.
:- mode - in(infinity)  = out(infinity)  is det.
:- mode - in            = out            is det.

:- func num(N)        + num(N)        = num(N) <= arithmetic.arithmetic(N).
:- mode in(neg_inf)   + in(neg_inf)   = out(neg_inf)   is det.
:- mode in(neg_inf)   + in(basic_num) = out(neg_inf)   is det.
:- mode in(neg_inf)   + in(pos_inf)   = out(_)         is failure.
:- mode in(basic_num) + in(neg_inf)   = out(neg_inf)   is det.
:- mode in(basic_num) + in(basic_num) = out(basic_num) is det.
:- mode in(basic_num) + in(pos_inf)   = out(pos_inf)   is det.
:- mode in(pos_inf)   + in(neg_inf)   = out(_)         is failure.
:- mode in(pos_inf)   + in(basic_num) = out(pos_inf)   is det.
:- mode in(pos_inf)   + in(pos_inf)   = out(pos_inf)   is det.
:- mode in(infinity)  + in(infinity)  = out(infinity)  is semidet.
:- mode in(infinity)  + in(basic_num) = out(infinity)  is det.
:- mode in(basic_num) + in(infinity)  = out(infinity)  is det.
:- mode in            + in(basic_num) = out            is det.
:- mode in(basic_num) + in            = out            is det.
:- mode in            + in            = out            is semidet.

:- func num(N)        - num(N)        = num(N) <= arithmetic.arithmetic(N).
:- mode in(neg_inf)   - in(neg_inf)   = out(_)         is failure.
:- mode in(neg_inf)   - in(basic_num) = out(neg_inf)   is det.
:- mode in(neg_inf)   - in(pos_inf)   = out(neg_inf)   is det.
:- mode in(basic_num) - in(neg_inf)   = out(pos_inf)   is det.
:- mode in(basic_num) - in(basic_num) = out(basic_num) is det.
:- mode in(basic_num) - in(pos_inf)   = out(neg_inf)   is det.
:- mode in(pos_inf)   - in(neg_inf)   = out(pos_inf)   is det.
:- mode in(pos_inf)   - in(basic_num) = out(pos_inf)   is det.
:- mode in(pos_inf)   - in(pos_inf)   = out(_)         is failure.
:- mode in(infinity)  - in(infinity)  = out(infinity)  is semidet.
:- mode in(infinity)  - in(basic_num) = out(infinity)  is det.
:- mode in(basic_num) - in(infinity)  = out(infinity)  is det.
:- mode in            - in(basic_num) = out            is det.
:- mode in(basic_num) - in            = out            is det.
:- mode in            - in            = out            is semidet.

:- func num(N)        * num(N)        = num(N) <= arithmetic.arithmetic(N).
:- mode in(neg_inf)   * in(neg_inf)   = out(pos_inf)   is det.
:- mode in(neg_inf)   * in(basic_num) = out(infinity)  is semidet.
:- mode in(neg_inf)   * in(pos_inf)   = out(neg_inf)   is det.
:- mode in(basic_num) * in(neg_inf)   = out(infinity)  is semidet.
:- mode in(basic_num) * in(basic_num) = out(basic_num) is det.
:- mode in(basic_num) * in(pos_inf)   = out(infinity)  is semidet.
:- mode in(pos_inf)   * in(neg_inf)   = out(neg_inf)   is det.
:- mode in(pos_inf)   * in(basic_num) = out(infinity)  is semidet.
:- mode in(pos_inf)   * in(pos_inf)   = out(pos_inf)   is det.
:- mode in(infinity)  * in(infinity)  = out(infinity)  is det.
:- mode in(infinity)  * in(basic_num) = out(infinity)  is semidet.
:- mode in(basic_num) * in(infinity)  = out(infinity)  is semidet.
:- mode in            * in            = out            is semidet.

:- func num(N)        / num(N)        = num(N) <= arithmetic.arithmetic(N).
:- mode in(neg_inf)   / in(neg_inf)   = out(_)         is failure.
:- mode in(neg_inf)   / in(basic_num) = out(infinity)  is semidet.
:- mode in(neg_inf)   / in(pos_inf)   = out(_)         is failure.
:- mode in(basic_num) / in(neg_inf)   = out(basic_num) is semidet.
:- mode in(basic_num) / in(basic_num) = out            is semidet.
:- mode in(basic_num) / in(pos_inf)   = out(basic_num) is semidet.
:- mode in(pos_inf)   / in(neg_inf)   = out(_)         is failure.
:- mode in(pos_inf)   / in(basic_num) = out(infinity)  is semidet.
:- mode in(pos_inf)   / in(pos_inf)   = out(_)         is failure.
:- mode in(infinity)  / in(infinity)  = out(infinity)  is failure.
:- mode in(infinity)  / in(basic_num) = out(infinity)  is semidet.
:- mode in(basic_num) / in(infinity)  = out(basic_num) is semidet.
:- mode in            / in(basic_num) = out            is semidet.
:- mode in(basic_num) / in            = out            is semidet.
:- mode in            / in            = out            is semidet.

:- pred num(N)        < num(N) <= arithmetic.arithmetic(N).
:- mode in(neg_inf)   < in(neg_inf)   is failure.
:- mode in(neg_inf)   < in(basic_num) is det.
:- mode in(neg_inf)   < in(pos_inf)   is det.
:- mode in(basic_num) < in(neg_inf)   is failure.
:- mode in(basic_num) < in(basic_num) is semidet.
:- mode in(basic_num) < in(pos_inf)   is det.
:- mode in(pos_inf)   < in(neg_inf)   is failure.
:- mode in(pos_inf)   < in(basic_num) is failure.
:- mode in(pos_inf)   < in(pos_inf)   is failure.
:- mode in            < in            is semidet.

:- pred num(N)        =< num(N) <= arithmetic.arithmetic(N).
:- mode in(neg_inf)   =< in(neg_inf)   is det.
:- mode in(neg_inf)   =< in(basic_num) is det.
:- mode in(neg_inf)   =< in(pos_inf)   is det.
:- mode in(basic_num) =< in(neg_inf)   is failure.
:- mode in(basic_num) =< in(basic_num) is semidet.
:- mode in(basic_num) =< in(pos_inf)   is det.
:- mode in(pos_inf)   =< in(neg_inf)   is failure.
:- mode in(pos_inf)   =< in(basic_num) is failure.
:- mode in(pos_inf)   =< in(pos_inf)   is det.
:- mode in            =< in            is semidet.

:- pred num(N)        > num(N) <= arithmetic.arithmetic(N).
:- mode in(neg_inf)   > in(neg_inf)   is failure.
:- mode in(neg_inf)   > in(basic_num) is failure.
:- mode in(neg_inf)   > in(pos_inf)   is failure.
:- mode in(basic_num) > in(neg_inf)   is det.
:- mode in(basic_num) > in(basic_num) is semidet.
:- mode in(basic_num) > in(pos_inf)   is failure.
:- mode in(pos_inf)   > in(neg_inf)   is det.
:- mode in(pos_inf)   > in(basic_num) is det.
:- mode in(pos_inf)   > in(pos_inf)   is failure.
:- mode in            > in            is semidet.


:- pred num(N)        >= num(N) <= arithmetic.arithmetic(N).
:- mode in(neg_inf)   >= in(neg_inf)   is det.
:- mode in(neg_inf)   >= in(basic_num) is failure.
:- mode in(neg_inf)   >= in(pos_inf)   is failure.
:- mode in(basic_num) >= in(neg_inf)   is det.
:- mode in(basic_num) >= in(basic_num) is semidet.
:- mode in(basic_num) >= in(pos_inf)   is failure.
:- mode in(pos_inf)   >= in(neg_inf)   is det.
:- mode in(pos_inf)   >= in(basic_num) is det.
:- mode in(pos_inf)   >= in(pos_inf)   is det.
:- mode in            >= in            is semidet.


:- pred num(N)       `in` {num(N), num(N)} <= arithmetic.arithmetic(N).
:- mode in(pos_inf)  `in` in(tuple(basic_num, basic_num)) is failure.
:- mode in(neg_inf)  `in` in(tuple(basic_num, basic_num)) is failure.
%:- mode in(infinity) `in` in(tuple(basic_num, basic_num)) is failure.
:- mode in           `in` in                              is semidet.

%-----------------------------------------------------------------------------%

:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module exception.

%-----------------------------------------------------------------------------%

number(N) = num(N).
number_from_float(F) = num(arithmetic.from_float(F)).
number_from_int(I) = num(arithmetic.from_int(I)).

basic(num(N)) = N.

basic(num(_)).

det_basic(N) = ( if X = basic(N) then X else throw({"det_basic/1", N}) ).

negative_infinity = neg_inf.
positive_infinity = pos_inf.

negative_infinity(neg_inf).
positive_infinity(pos_inf).

infinity(neg_inf).
infinity(pos_inf).

negative(N) :- N < zero.
positive(N) :- N > zero.

one = num(arithmetic.one).
zero = num(arithmetic.zero).

abs(neg_inf) = pos_inf.
abs(num(N))  = num(arithmetic.abs(N)).
abs(pos_inf) = pos_inf.

+ N = N.

- neg_inf = pos_inf.
- num(N)  = num(arithmetic.'-'(N)).
- pos_inf = neg_inf.

neg_inf + neg_inf = neg_inf.
neg_inf + num(_)  = neg_inf.
neg_inf + pos_inf = _ :- fail.
num(_)  + neg_inf = neg_inf.
num(N)  + num(M)  = num(N `arithmetic.'+'` M).
num(_)  + pos_inf = pos_inf.
pos_inf + neg_inf = _ :- fail.
pos_inf + num(_)  = pos_inf.
pos_inf + pos_inf = pos_inf.

A - B = A + (-B).

neg_inf * neg_inf = pos_inf.
neg_inf * num(N)  = R :- ( if N = z then fail else R = flip(N, neg_inf) ).
neg_inf * pos_inf = neg_inf.
num(N)  * neg_inf = R :- ( if N = z then fail else R = flip(N, neg_inf) ).
num(N)  * num(M)  = num(N `arithmetic.'*'` M).
num(N)  * pos_inf = R :- ( if N = z then fail else R = flip(N, pos_inf) ).
pos_inf * neg_inf = neg_inf.
pos_inf * num(N)  = R :- ( if N = z then fail else R = flip(N, pos_inf) ).
pos_inf * pos_inf = pos_inf.

neg_inf / neg_inf = _ :- fail.
neg_inf / num(N)  = R :- ( if N = z then fail else R = flip(N, neg_inf) ).
neg_inf / pos_inf = _ :- fail.
num(N)  / neg_inf = R :- ( if N = z then fail else R = zero ).
num(N)  / num(M)  = R :- ( if N = z, M = z then fail else
                           if M = z then R = flip(N, pos_inf)
                                       else R = num(N `arithmetic.'/'` M) ).
num(N)  / pos_inf = R :- ( if N = z then fail else R = zero ).
pos_inf / neg_inf = _ :- fail.
pos_inf / num(N)  = R :- ( if N = z then fail else R = flip(N, pos_inf) ).
pos_inf / pos_inf = _ :- fail.


:- func z = N <= arithmetic.arithmetic(N).

z = arithmetic.zero.


:- func flip(N, num(N)) = num(N) <= arithmetic.arithmetic(N).
:- mode flip(in, in(infinity)) = out(infinity) is det.
:- mode flip(in, in(basic_num)) = out is failure.
:- mode flip(in, in) = out is semidet.

flip(X, neg_inf) = ( if X `arithmetic.'<'` z then pos_inf else neg_inf ).
flip(X, pos_inf) = ( if X `arithmetic.'<'` z then neg_inf else pos_inf ).


neg_inf < neg_inf :- fail.
neg_inf < num(_).
neg_inf < pos_inf.
num(_)  < neg_inf :- fail.
num(N)  < num(M) :- N `arithmetic.'<'` M.
num(_)  < pos_inf.
pos_inf < neg_inf :- fail.
pos_inf < num(_) :- fail.
pos_inf < pos_inf :- fail.

neg_inf =< neg_inf.
neg_inf =< num(_).
neg_inf =< pos_inf.
num(_)  =< neg_inf :- fail.
num(N)  =< num(M) :- N `arithmetic.'=<'` M.
num(_)  =< pos_inf.
pos_inf =< neg_inf :- fail.
pos_inf =< num(_) :- fail.
pos_inf =< pos_inf.

N > M :- M < N.

N >= M :- M =< N.

N `in` {Lo, Hi} :- Lo < N, N =< Hi.

%-----------------------------------------------------------------------------%
:- end_module inf_arithmetic.
%-----------------------------------------------------------------------------%

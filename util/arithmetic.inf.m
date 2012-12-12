%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: arithmetic.inf.m.
% Main author: schwering.
%
% Extension of numbers with positive and negative infinity.
%
%-----------------------------------------------------------------------------%

:- module arithmetic.inf.

:- interface.

%-----------------------------------------------------------------------------%

:- type num(N).

%-----------------------------------------------------------------------------%

:- func number(N) = num(N) <= arithmetic(N).

:- func negative_infinity = num(N) <= arithmetic(N).
:- func positive_infinity = num(N) <= arithmetic(N).

:- pred negative_infinity(num(N)::in) is semidet.
:- pred positive_infinity(num(N)::in) is semidet.

:- func        + num(N) = num(N) <= arithmetic(N).
:- func        - num(N) = num(N) <= arithmetic(N).

:- func num(N) + num(N) = num(N) <= arithmetic(N).
:- mode in     + in     = out is semidet.

:- func num(N) - num(N) = num(N) <= arithmetic(N).
:- mode in     - in     = out is semidet.

:- func num(N) * num(N) = num(N) <= arithmetic(N).
:- mode in     * in     = out is semidet.

:- func num(N) / num(N) = num(N) <= arithmetic(N).
:- mode in     / in     = out is semidet.

:- pred (num(N)::in) <  (num(N)::in) is semidet <= arithmetic(N).
:- pred (num(N)::in) =< (num(N)::in) is semidet <= arithmetic(N).
:- pred (num(N)::in) >  (num(N)::in) is semidet <= arithmetic(N).
:- pred (num(N)::in) >= (num(N)::in) is semidet <= arithmetic(N).

%-----------------------------------------------------------------------------%

:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- type num(N) ---> neg_inf ; num(N) ; pos_inf.
:- inst infinity ---> neg_inf ; pos_inf.

%-----------------------------------------------------------------------------%

number(N) = num(N).

negative_infinity = neg_inf.
positive_infinity = pos_inf.

negative_infinity(neg_inf).
positive_infinity(pos_inf).


+ N = N.


- neg_inf = pos_inf.
- num(N)  = num(-N).
- pos_inf = neg_inf.


neg_inf + neg_inf = neg_inf.
neg_inf + num(_)  = neg_inf.
neg_inf + pos_inf = _ :- fail.
num(_)  + neg_inf = neg_inf.
num(N)  + num(M)  = num(N + M).
num(_)  + pos_inf = pos_inf.
pos_inf + neg_inf = _ :- fail.
pos_inf + num(_)  = pos_inf.
pos_inf + pos_inf = pos_inf.


A - B = A `inf.'+'` inf.'-'(B).


neg_inf * neg_inf = pos_inf.
neg_inf * num(N)  = R :- ( if N = zero then fail else R = flip(N, neg_inf) ).
neg_inf * pos_inf = neg_inf.
num(N)  * neg_inf = R :- ( if N = zero then fail else R = flip(N, neg_inf) ).
num(N)  * num(M)  = num(N * M).
num(N)  * pos_inf = R :- ( if N = zero then fail else R = flip(N, pos_inf) ).
pos_inf * neg_inf = neg_inf.
pos_inf * num(N)  = R :- ( if N = zero then fail else R = flip(N, pos_inf) ).
pos_inf * pos_inf = pos_inf.


neg_inf / neg_inf = _ :- fail.
neg_inf / num(N)  = R :- ( if N = zero then fail else R = flip(N, neg_inf) ).
neg_inf / pos_inf = _ :- fail.
num(N)  / neg_inf = R :- ( if N = zero then fail else R = num(zero) ).
num(N)  / num(M)  = R :- ( if N = zero, M = zero then fail else
                           if M = zero then R = flip(N, pos_inf)
                                       else R = num(N / M) ).
num(N)  / pos_inf = R :- ( if N = zero then fail else R = num(zero) ).
pos_inf / neg_inf = _ :- fail.
pos_inf / num(N)  = R :- ( if N = zero then fail else R = flip(N, pos_inf) ).
pos_inf / pos_inf = _ :- fail.


:- func flip(N, num(N)) = num(N) <= arithmetic(N).
:- mode flip(in, in(infinity)) = out(infinity) is det.
:- mode flip(in, in) = out is semidet.

flip(X, neg_inf) = ( if X < zero then pos_inf else neg_inf ).
flip(X, pos_inf) = ( if X < zero then neg_inf else pos_inf ).


neg_inf < neg_inf :- fail.
neg_inf < num(_).
neg_inf < pos_inf.
num(_)  < neg_inf :- fail.
num(N)  < num(M) :- N < M.
num(_)  < pos_inf.
pos_inf < neg_inf :- fail.
pos_inf < num(_) :- fail.
pos_inf < neg_inf :- fail.


neg_inf =< neg_inf.
neg_inf =< num(_).
neg_inf =< pos_inf.
num(_)  =< neg_inf :- fail.
num(N)  =< num(M) :- N =< M.
num(_)  =< pos_inf.
pos_inf =< neg_inf :- fail.
pos_inf =< num(_) :- fail.
pos_inf =< neg_inf.


N > M :- M `inf.'<'` N.


N >= M :- M `inf.'=<'` N.

%-----------------------------------------------------------------------------%
:- end_module arithmetic.inf.
%-----------------------------------------------------------------------------%

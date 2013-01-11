%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: util.inf_arithmetic.test.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module util.inf_arithmetic.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module require.
:- import_module util.arithmetic.impl.

%-----------------------------------------------------------------------------%

:- func n(float) = num(float).

n(F) = number(F).


test(!IO) :-
    some [X] ( if + n(3.0) = X, X \= n(3.0) then throw({"3 \\= 3", X}) else true ),
    some [X] ( if + n(-3.0) = X, X \= n(-3.0) then throw({"3 \\= -3", X}) else true ),
    some [X] ( if + positive_infinity `with_type` num(float) = X, X \= positive_infinity then throw({"+ +inf \\= +inf", X}) else true ),
    some [X] ( if + negative_infinity `with_type` num(float) = X, X \= negative_infinity then throw({"+ -inf \\= -inf", X}) else true ),
    %
    some [X] ( if - n(3.0) = X, X \= n(-3.0) then throw({"3 \\= -3", X}) else true ),
    some [X] ( if - n(-3.0) = X, X \= n(3.0) then throw({"-3 \\= 3", X}) else true ),
    some [X] ( if - positive_infinity `with_type` num(float) = X, X \= negative_infinity then throw({"- +inf \\= -inf", X}) else true ),
    some [X] ( if - negative_infinity `with_type` num(float) = X, X \= positive_infinity then throw({"- -inf \\= +inf", X}) else true ),
    %
    some [X] ( if n(3.0) + n(4.0) = X, X \= n(7.0) then throw({"3 + 4 \\= 7", X}) else true ),
    some [X] ( if positive_infinity + n(4.0) = X, X \= positive_infinity then throw({"+inf + 4 \\= +inf", X}) else true ),
    some [X] ( if negative_infinity + n(4.0) = X, X \= negative_infinity then throw({"-inf + 4 \\= -inf", X}) else true ),
    some [X] ( if n(4.0) + positive_infinity = X, X \= positive_infinity then throw({"4 + +inf \\= +inf", X}) else true ),
    some [X] ( if n(4.0) + negative_infinity = X, X \= negative_infinity then throw({"4 + -inf \\= -inf", X}) else true ),
    some [X] ( if positive_infinity `with_type` num(float) + positive_infinity = X, X \= positive_infinity then throw({"+inf + +inf \\= +inf", X}) else true ),
    some [X] ( if negative_infinity `with_type` num(float) + negative_infinity = X, X \= negative_infinity then throw({"-inf + -inf \\= -inf", X}) else true ),
    some [X] ( if positive_infinity `with_type` num(float) + negative_infinity = X then throw({"+inf + -inf defined", X}) else true ),
    some [X] ( if negative_infinity `with_type` num(float) + positive_infinity = X then throw({"-inf + +inf defined", X}) else true ),
    %
    some [X] ( if n(3.0) - n(4.0) = X, X \= n(-1.0) then throw({"3 - 4 \\= -1", X}) else true ),
    some [X] ( if positive_infinity - n(4.0) = X, X \= positive_infinity then throw({"+inf - 4 \\= +inf", X}) else true ),
    some [X] ( if negative_infinity - n(4.0) = X, X \= negative_infinity then throw({"-inf - 4 \\= -inf", X}) else true ),
    some [X] ( if n(4.0) - positive_infinity = X, X \= negative_infinity then throw({"4 - +inf \\= -inf", X}) else true ),
    some [X] ( if n(4.0) - negative_infinity = X, X \= positive_infinity then throw({"4 - -inf \\= +inf", X}) else true ),
    some [X] ( if positive_infinity `with_type` num(float) - positive_infinity = X then throw({"+inf + +inf defined", X}) else true ),
    some [X] ( if negative_infinity `with_type` num(float) - negative_infinity = X then throw({"-inf + -inf defined", X}) else true ),
    some [X] ( if positive_infinity `with_type` num(float) - negative_infinity = X, X \= positive_infinity then throw({"+inf - -inf \\= +inf", X}) else true ),
    some [X] ( if negative_infinity `with_type` num(float) - positive_infinity = X, X \= negative_infinity then throw({"-inf - +inf \\= -inf", X}) else true ),
    %
    some [X] ( if n(3.0) * n(4.0) = X, X \= n(12.0) then throw({"3 * 4 \\= 12", X}) else true ),
    some [X] ( if n(-3.0) * n(4.0) = X, X \= n(-12.0) then throw({"-3 * 4 \\= -12", X}) else true ),
    some [X] ( if n(3.0) * n(-4.0) = X, X \= n(-12.0) then throw({"3 * -4 \\= -12", X}) else true ),
    some [X] ( if n(-3.0) * n(-4.0) = X, X \= n(12.0) then throw({"3 * 4 \\= 12", X}) else true ),
    some [X] ( if positive_infinity * n(3.0) = X, X \= positive_infinity then throw({"+inf * 3 \\= +inf", X}) else true ),
    some [X] ( if positive_infinity * n(-3.0) = X, X \= negative_infinity then throw({"inf * -3 \\= -inf", X}) else true ),
    some [X] ( if negative_infinity * n(-3.0) = X, X \= positive_infinity then throw({"-inf * -3 \\= +inf", X}) else true ),
    some [X] ( if negative_infinity * n(3.0) = X, X \= negative_infinity then throw({"-inf * 3 \\= -inf", X}) else true ),
    some [X] ( if n(3.0) * positive_infinity = X, X \= positive_infinity then throw({"3 * +inf \\= +inf", X}) else true ),
    some [X] ( if n(-3.0) * positive_infinity = X, X \= negative_infinity then throw({"-3 * inf \\= -inf", X}) else true ),
    some [X] ( if n(-3.0) * negative_infinity = X, X \= positive_infinity then throw({"-3 * -inf \\= +inf", X}) else true ),
    some [X] ( if n(3.0) * negative_infinity = X, X \= negative_infinity then throw({"3 * -inf \\= -inf", X}) else true ),
    some [X] ( if positive_infinity `with_type` num(float) * positive_infinity = X, X \= positive_infinity then throw({"+inf * +inf \\= +inf", X}) else true ),
    some [X] ( if positive_infinity `with_type` num(float) * negative_infinity = X, X \= negative_infinity then throw({"+inf * -inf \\= -inf", X}) else true ),
    some [X] ( if negative_infinity `with_type` num(float) * positive_infinity = X, X \= negative_infinity then throw({"-inf * +inf \\= -inf", X}) else true ),
    some [X] ( if negative_infinity `with_type` num(float) * negative_infinity = X, X \= positive_infinity then throw({"-inf * -inf \\= +inf", X}) else true ),
    %
    some [X] ( if n(3.0) / n(4.0) = X, X \= n(0.75) then throw({"3 / 4 \\= 0.75", X}) else true ),
    some [X] ( if n(0.0) / n(4.0) = X, X \= n(0.0) then throw({"0 / 4 \\= 0", X}) else true ),
    some [X] ( if n(0.0) / n(0.0) = X then throw({"0 / 0 defined", X}) else true ),
    some [X] ( if n(3.0) / n(0.0) = X, X \= positive_infinity then throw({"3 / 0 \\= +inf", X}) else true ),
    some [X] ( if n(-3.0) / n(0.0) = X, X \= negative_infinity then throw({"-3 / 0 \\= -inf", X}) else true ),
    some [X] ( if n(3.0) / positive_infinity = X, X \= n(0.0) then throw({"3 / +inf \\= 0", X}) else true ),
    some [X] ( if n(3.0) / negative_infinity = X, X \= n(0.0) then throw({"3 / +inf \\= 0", X}) else true ),
    some [X] ( if n(-3.0) / positive_infinity = X, X \= n(0.0) then throw({"-3 / +inf \\= 0", X}) else true ),
    some [X] ( if n(-3.0) / negative_infinity = X, X \= n(0.0) then throw({"-3 / +inf \\= 0", X}) else true ),
    some [X] ( if positive_infinity / n(3.0) = X, X \= positive_infinity then throw({"+inf / 3 \\= +inf", X}) else true ),
    some [X] ( if positive_infinity / n(-3.0) = X, X \= negative_infinity then throw({"+inf / -3 \\= -inf", X}) else true ),
    some [X] ( if negative_infinity / n(3.0) = X, X \= negative_infinity then throw({"-inf / 3 \\= -inf", X}) else true ),
    some [X] ( if negative_infinity / n(-3.0) = X, X \= positive_infinity then throw({"-inf / -3 \\= +inf", X}) else true ),
    %
    ( if not positive_infinity(positive_infinity `with_type` num(float)) then throw("not pos_inf(pos_inf)") else true ),
    ( if     positive_infinity(negative_infinity `with_type` num(float)) then throw("not pos_inf(neg_inf)") else true ),
    ( if not negative_infinity(negative_infinity `with_type` num(float)) then throw("not neg_inf(neg_inf)") else true ),
    ( if     negative_infinity(positive_infinity `with_type` num(float)) then throw("not neg_inf(pos_inf)") else true ),
    %
    ( if n(3.0) < n(4.0) then true else throw("3 < 4") ),
    ( if n(3.0) =< n(4.0) then true else throw("3 =< 4") ),
    ( if not n(3.0) < n(-4.0) then true else throw("not 3 < 4") ),
    ( if n(-5.0) =< n(-4.0) then true else throw("-5 =< -4") ),
    ( if not n(4.0) =< n(3.0) then true else throw("not 4 >= 3") ),
    ( if not n(4.0) =< negative_infinity then true else throw("not 4 >= -inf") ),
    ( if not n(4.0) < negative_infinity then true else throw("not 4 > -inf") ),
    ( if not n(4.0) =< n(3.0) then true else throw("not 4 >= 3") ),
    ( if not n(4.0) >= positive_infinity then true else throw("not 4 >= +inf") ),
    ( if not n(4.0) > positive_infinity then true else throw("not 4 > +inf") ),
    ( if n(4.0) >= negative_infinity then true else throw("4 >= -inf") ),
    ( if n(4.0) > negative_infinity then true else throw("4 > -inf") ),
    ( if n(4.0) < positive_infinity then true else throw("4 < +inf") ),
    ( if positive_infinity > n(4.0) then true else throw("+inf > 4") ),
    ( if positive_infinity >= n(4.0) then true else throw("+inf >= 4") ),
    ( if not n(4.0) > positive_infinity then true else throw("not 4 > +inf") ),
    ( if not n(4.0) >= positive_infinity then true else throw("not 4 >= +inf") ),
    ( if not n(4.0) < negative_infinity then true else throw("not 4 < -inf") ),
    ( if not n(4.0) =< negative_infinity then true else throw("not 4 =< -inf") ),
    ( if negative_infinity `with_type` num(float) < positive_infinity then true else throw("-inf < +inf") ),
    ( if negative_infinity `with_type` num(float) =< positive_infinity then true else throw("-inf < +inf") ),
    ( if not positive_infinity `with_type` num(float) < positive_infinity then true else throw("+inf < +inf") ),
    ( if not negative_infinity `with_type` num(float) < negative_infinity then true else throw("-inf < -inf") ),
    ( if positive_infinity `with_type` num(float) =< positive_infinity then true else throw("+inf =< +inf") ),
    ( if negative_infinity `with_type` num(float) =< negative_infinity then true else throw("-inf =< -inf") ),
    true.

%-----------------------------------------------------------------------------%
:- end_module util.inf_arithmetic.test.
%-----------------------------------------------------------------------------%

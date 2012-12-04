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
% Note that epsilon/1 has an argument, the number whose nearest bigger neighbor
% we look for.  The reason is that for some imprecise number types for large X
% we have the paradox X + epsilon = X.  This holds for floats, for example.
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
    func epsilon(N) = N, % next bigger neighbor of N
    func from_float(float) = N,
    func from_int(int) = N,
    func to_float(N) = float,
    pred (N::in) < (N::in) is semidet,
    pred (N::in) > (N::in) is semidet,
    pred (N::in) =< (N::in) is semidet,
    pred (N::in) >= (N::in) is semidet
].

%-----------------------------------------------------------------------------%

:- func pow(N, int) = N <= arithmetic(N).

%-----------------------------------------------------------------------------%

    % bin_search(F, XMin, XMax, Y, X) determines the value X that such that F(X)
    % matches Y with accuracy 4*eps for a monotone function F.
    %
    % If F is not increasing, it is inverted.
    %
    % Now first of all, XMid = (XMin + XMax) / 2 may be imprecise by more than
    % one epsilon. In this case we simply increment XMin repeatedly by one
    % epsilon. This is done in halfway/2.
    %
    % One problem arises because when F has a slope > 1, [XMin, XMax] might have
    % narrowed down the correct X value with an accuracy of eps, i.e.,
    % XMax - XMin =< eps, but still F(XMax) - F(XMin) > eps.
    %
    % When
    %   XMin =< X =< XMax and
    %   XMax - XMin =< eps
    % for X being the answer we look for, this implies that
    %   X - eps =< XMin
    %   X + eps >= XMax.
    %
    % Now since F is monotone and increasing, this implies
    %   F(XMin) =< F(X) =< F(XMax),
    % but
    %   F(X - eps) =< F(XMin)
    %   F(X + eps) >= F(XMax).
    % When the latter two inequailities hold, we're happy and accept the as
    % precise enough. That's for the case where the slope is > 1.
    %
    % For the other case, where the slope is < 1, we impose the conditions
    %   F(X) - eps =< F(XMin)
    %   F(X) + eps >= F(XMax).
    %
    % From
    %   X - eps =< XMin
    %   X + eps >= XMax.
    % we get
    %   XMax - XMin =< 2 * eps
    % by multiplying the first line with (-1) and adding the result to the
    % second line. Thus, the precision of our computation is 2 * eps.
    % (TODO Does this really apply to the conditions for slope > 1?)
    %
:- pred bin_search(func(X) = Y, X, X, Y, X) <= (arithmetic(X), arithmetic(Y)).
:- mode bin_search(in(func(in) = out is det), in, in, in, out) is semidet.
:- mode bin_search(in(func(in) = out is semidet), in, in, in, out) is semidet.

%-----------------------------------------------------------------------------%

:- include_module impl.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.

%-----------------------------------------------------------------------------%

:- func two = N <= arithmetic(N).

:- pragma type_spec(two/0, N = float).

two = from_int(2).

:- pragma type_spec(pow/2, N = float).

pow(X, N) = (    if N = 0         then  one
            else if N `int.'>'` 0 then  X * pow(X, N - 1)
            else                        one / pow(X, -N) ).

%-----------------------------------------------------------------------------%

:- func halfway(N, N) = N <= arithmetic(N).

:- pragma type_spec(halfway/2, N = float).

:- use_module float.

halfway(X, Y) = Z :-
   ( if X = Y
     then Z = X
     else
        H1 = (X + Y) / two,
        H2 = X + epsilon(X),
        (    if X < H1, H1  < Y then Z = H1
        else if X < H2, H2 =< Y then Z = H2
        else throw({"arithmetic.halfway: X+eps > Z?", X, Y, H1, H2}) )
   ).

%-----------------------------------------------------------------------------%

:- pragma type_spec(bin_search/5, (X = float, Y = float)).

bin_search(F, XMin, XMax, YGoal, X) :-
    if F(XMin) > F(XMax) then
        bin_search_2(func(X1) = -F(X1) is semidet, XMin, XMax, -YGoal, X)
    else
        bin_search_2(F, XMin, XMax, YGoal, X).


:- pred bin_search_2(func(X) = Y, X, X, Y, X) <= (arithmetic(X), arithmetic(Y)).
:- mode bin_search_2(in(func(in) = out is det), in, in, in, out) is semidet.
:- mode bin_search_2(in(func(in) = out is semidet), in, in, in, out) is semidet.

:- pragma type_spec(bin_search_2/5, (X = float, Y = float)).

:- import_module io.

bin_search_2(F, XMin, XMax, YGoal, X) :-
    XMid = halfway(XMin, XMax),
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
%            write_string("f(Xmin-) = ", !IO), write(FXMin - two * epsilon, !IO), nl(!IO),
%            write_string("f(Xmin)  = ", !IO), write(FXMin, !IO), nl(!IO),
%            write_string("f(Xmid)  = ", !IO), write(FXMid, !IO), nl(!IO),
%            write_string("ygoal    = ", !IO), write(YGoal, !IO), nl(!IO),
%            write_string("f(Xmax)  = ", !IO), write(FXMax, !IO), nl(!IO),
%            write_string("f(Xmax+) = ", !IO), write(FXMax + two * epsilon, !IO), nl(!IO),
%            write_string("2eps = ", !IO), write(2.0 `float.'*'` float.epsilon, !IO), nl(!IO),
%            write_string("diff = ", !IO), write(XMax - XMin, !IO), nl(!IO),
%            write_string("minu = ", !IO), write(XMax - XMin - two * epsilon, !IO), nl(!IO),
%            write_string("f-diff = ", !IO), write(FXMax - FXMin, !IO), nl(!IO)
%        else true ),
%        true
%    ),
    (
        if
            F(XMin) =< YGoal, YGoal =< F(XMax),
            F(XMid) - epsilon(F(XMid)) =< YGoal,
            F(XMid) + epsilon(F(XMid)) >= YGoal
        then
            X = XMid
        else if
            F(XMin) =< YGoal, YGoal =< F(XMax),
            F(XMid - epsilon(XMid)) =< F(XMin),
            F(XMid + epsilon(XMid)) >= F(XMax)
        then
            X = XMid
        else if
            XMin = XMax
        then
            fail
        else if
            F(XMid) < YGoal then bin_search_2(F, XMid, XMax, YGoal, X)
        else /* if F(XMid) > YGoal */
            bin_search_2(F, XMin, XMid, YGoal, X)
    ).

%-----------------------------------------------------------------------------%
:- end_module arithmetic.
%-----------------------------------------------------------------------------%

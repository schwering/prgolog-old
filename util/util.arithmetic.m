%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012-2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: util.arithmetic.m.
% Main author: schwering.
%
% Typeclass for arithmetic operations.
%
% Note that epsilon/1 has an argument, the number whose nearest bigger neighbor
% we look for.  The reason is that for some imprecise number types for large X
% we have the paradox X + epsilon = X.  This holds for floats, for example.
%
%-----------------------------------------------------------------------------%

:- module util.arithmetic.

:- interface.

:- import_module list.
:- import_module solutions.

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

:- func min(N, N) = N <= arithmetic(N).
:- func max(N, N) = N <= arithmetic(N).

:- func optimize(func(N, N) = N, pred(T), func(T) = N) = N.
:- mode optimize(in, in(pred(out) is nondet), in) = out is semidet.
:- mode optimize(in, in(pred(out) is multi), in) = out is det.

:- func minimize(pred(N)) = N <= arithmetic(N).
:- mode minimize(in(pred(out) is nondet)) = out is semidet.
:- mode minimize(in(pred(out) is multi)) = out is det.

:- func minimize(pred(T), func(T) = N) = N <= arithmetic(N).
:- mode minimize(in(pred(out) is nondet), in) = out is semidet.
:- mode minimize(in(pred(out) is multi), in) = out is det.

:- func maximize(pred(N)) = N <= arithmetic(N).
:- mode maximize(in(pred(out) is nondet)) = out is semidet.
:- mode maximize(in(pred(out) is multi)) = out is det.

:- func maximize(pred(T), func(T) = N) = N <= arithmetic(N).
:- mode maximize(in(pred(out) is nondet), in) = out is semidet.
:- mode maximize(in(pred(out) is multi), in) = out is det.

%-----------------------------------------------------------------------------%

:- include_module impl.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, string.
:- import_module int.
:- import_module std_util.

%-----------------------------------------------------------------------------%

:- func two = N <= arithmetic(N).

two = from_int(2).

pow(X, N) = (    if N = 0         then  one
            else if N `int.'>'` 0 then  X * pow(X, N - 1)
            else                        one / pow(X, -N) ).

%-----------------------------------------------------------------------------%

:- func halfway(N, N) = N <= arithmetic(N).

halfway(X, Y) = Z :-
   ( if X = Y
     then Z = X
     else
        H1 = (X + Y) / two,
        H2 = X + epsilon(X),
        (    if X < H1, H1  < Y then Z = H1
        else if X < H2, H2 =< Y then Z = H2
        else unexpected($module, string.format("%s: %s + eps > %s? %s, %s",
                        [s($pred), s(string(X)), s(string(Y)),
                         s(string(H1)), s(string(H2))]))
        )
   ).

%-----------------------------------------------------------------------------%

bin_search(F, XMin, XMax, YGoal, X) :-
    if F(XMin) > F(XMax) then
        bin_search_2(func(X1) = -F(X1) is semidet, XMin, XMax, -YGoal, X)
    else
        bin_search_2(F, XMin, XMax, YGoal, X).


:- pred bin_search_2(func(X) = Y, X, X, Y, X) <= (arithmetic(X), arithmetic(Y)).
:- mode bin_search_2(in(func(in) = out is det), in, in, in, out) is semidet.
:- mode bin_search_2(in(func(in) = out is semidet), in, in, in, out) is semidet.

%:- import_module io.

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


min(X, Y) = ( if X < Y then X else Y ).
max(X, Y) = ( if X > Y then X else Y ).


optimize(O, P, F) = Y :-
    [X|Xs] = solutions(P),
    Y = foldl(func(X1, X2) = O(F(X1), X2), Xs, F(X)).

minimize(P) = minimize(P, id).

minimize(P, F) = optimize(min, P, F).

maximize(P) = maximize(P, id).

maximize(P, F) = optimize(max, P, F).

%-----------------------------------------------------------------------------%

:- pragma type_spec(two/0, N = float).
:- pragma type_spec(pow/2, N = float).
:- pragma type_spec(halfway/2, N = float).
:- pragma type_spec(bin_search/5, (X = float, Y = float)).
:- pragma type_spec(bin_search_2/5, (X = float, Y = float)).

%-----------------------------------------------------------------------------%
:- end_module util.arithmetic.
%-----------------------------------------------------------------------------%

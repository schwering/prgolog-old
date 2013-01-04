%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: arithmetic.impl.test.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module arithmetic.impl.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test_bin_search_rat(io::di, io::uo) is det.
:- pred test_bin_search_rat_sq(io::di, io::uo) is det.
:- pred test_bin_search_float(io::di, io::uo) is det.
:- pred test_bin_search_float_inv(io::di, io::uo) is det.
:- pred test_bin_search_float_sq(io::di, io::uo) is det.
:- pred test_bin_search_float_sq2(io::di, io::uo) is det.
:- pred test_bin_search_float_sqrt(io::di, io::uo) is det.
:- pred test_bin_search_float_fail(io::di, io::uo) is det.
:- pred test_optimize_float(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module list.
:- use_module math.
:- import_module prgolog.nice.
:- import_module string.
:- import_module rat.
:- import_module require.
:- import_module std_util.

%-----------------------------------------------------------------------------%

:- pred in_ball(int::in, N::in, N::in) is semidet <= arithmetic(N).

in_ball(N, X, Y) :-
    abs(X - Y) =< from_int(N) * epsilon(X).

:- pred in_eps_ball(N::in, N::in) is semidet <= arithmetic(N).

in_eps_ball(X, Y) :- in_ball(1, X, Y).


:- pred in_2eps_ball(func(X) = Y, X, Y) <= (arithmetic(X), arithmetic(Y)).
:- mode in_2eps_ball(in(func(in) = out is det), in, in) is semidet.
:- mode in_2eps_ball(in(func(in) = out is semidet), in, in) is semidet.

in_2eps_ball(F, X, Y) :-
    F(X - two * epsilon(X)) =< Y,
    F(X + two * epsilon(X)) >= Y.

%-----------------------------------------------------------------------------%

test_bin_search_rat(!IO) :-
    F = (func(X) = rat(1, 1) `rat.'*'` X is det),
    Goal = rat(8, 7),
    Min = rat(0),
    Max = rat(5),
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_ball(2, F(Found), Goal) then true else throw({F(Found), Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_rat_sq(!IO) :-
    F = (func(X) = X `rat.'*'` X is det),
    Goal = rat(8, 7),
    Min = rat(0),
    Max = rat(5),
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_ball(2, F(Found), Goal) then true else throw({F(Found), Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_float(!IO) :-
    F = (func(X) = X is det),
    Goal = 8.0 `float.'/'` 7.0,
    Min = 0.0,
    Max = 5.0,
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_ball(2, F(Found), Goal) then true else throw({F(Found), Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_float_inv(!IO) :-
    F = (func(X) = -2.5 `float.'+'` X is det),
    Goal = 8.0 `float.'/'` 7.0,
    Min = 0.0,
    Max = 5.0,
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_ball(2, F(Found), Goal) then true else throw({F(Found), Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_float_sq(!IO) :-
    F = (func(X) = X `float.'*'` X is det),
    Goal = math.e,
    Min = 0.0,
    Max = 5.0,
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_ball(2, F(Found), Goal) then true else throw({F(Found), Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_float_sq2(!IO) :-
    F = (func(X) = X `float.'*'` X is det),
    Goal = math.pi,
    Min = 0.0,
    Max = 5.0,
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_ball(2, F(Found), Goal) then true else throw({F(Found), Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_float_sqrt(!IO) :-
    F = (func(X) = math.sqrt(X) is det),
    Goal = math.pi,
    Min = 0.0,
    Max = 16.0,
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_ball(2, F(Found), Goal) then true else throw({F(Found), Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_float_fail(!IO) :-
    F = (func(X) = math.sqrt(X) is det),
    Goal = 5.0,
    Min = 0.0,
    Max = 16.0,
    ( if bin_search(F, Min, Max, Goal, _Found)
      then throw({"found unexpectedly", Goal, F(Min), F(Max)})
      else true
    ),
    true.

test_optimize_float(!IO) :-
    some [Min] ( if Min = min(3.0, 4.0), Min \= 3.0 then throw({"min", 3.0, Min}) else true ),
    some [Max] ( if Max = max(3.0, 4.0), Max \= 4.0 then throw({"max", 4.0, Max}) else true ),
    P = ( pred(X::out) is multi :- X = 100.0 ; X = 3.0 ; X = -5.0 ; X = 10.0 ),
    some [Min] ( if Min = minimize(P, id), Min \= -5.0 then throw({"minimize", -5.0, Min}) else true ),
    some [Max] ( if Max = maximize(P, id), Max \= 100.0 then throw({"maximize", -5.0, Max}) else true ),
    true.

%-----------------------------------------------------------------------------%
:- end_module arithmetic.impl.test.
%-----------------------------------------------------------------------------%

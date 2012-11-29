%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: arithmetic.test.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module arithmetic.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test_bin_search_rat(io::di, io::uo) is det.
:- pred test_bin_search_rat_sq(io::di, io::uo) is det.
:- pred test_bin_search_float(io::di, io::uo) is det.
:- pred test_bin_search_float_inv(io::di, io::uo) is det.
:- pred test_bin_search_float_sq(io::di, io::uo) is det.
:- pred test_bin_search_float_sq2(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module arithmetic.
:- import_module bool.
:- import_module exception.
:- import_module list.
:- use_module math.
:- import_module prgolog.nice.
:- import_module string.
:- import_module rat.
:- import_module require.

%-----------------------------------------------------------------------------%

test_bin_search_rat(!IO) :-
    F = (func(X) = rat(1, 1) `rat.'*'` X is det),
    Goal = rat(8, 7),
    Min = rat(0),
    Max = rat(5),
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_2eps_ball(F, Found, Goal) then true else throw({Found, Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_rat_sq(!IO) :-
    F = (func(X) = X `rat.'*'` X is det),
    Goal = rat(8, 7),
    Min = rat(0),
    Max = rat(5),
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_2eps_ball(F, Found, Goal) then true else throw({Found, Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_float(!IO) :-
    F = (func(X) = X is det),
    Goal = 8.0 `float.'/'` 7.0,
    Min = 0.0,
    Max = 5.0,
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_2eps_ball(F, Found, Goal) then true else throw({Found, Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_float_inv(!IO) :-
    F = (func(X) = -2.5 `float.'+'` X is det),
    Goal = 8.0 `float.'/'` 7.0,
    Min = 0.0,
    Max = 5.0,
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_2eps_ball(F, Found, Goal) then true else throw({Found, Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_float_sq(!IO) :-
    F = (func(X) = X `float.'*'` X is det),
    Goal = math.e,
    Min = 0.0,
    Max = 5.0,
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_2eps_ball(F, Found, Goal) then true else throw({Found, Goal}) )
      else throw({"failed", Goal})
    ),
    true.

test_bin_search_float_sq2(!IO) :-
    F = (func(X) = X `float.'*'` X is det),
    Goal = math.pi,
    Min = 0.0,
    Max = 5.0,
    ( if bin_search(F, Min, Max, Goal, Found)
      then ( if in_2eps_ball(F, Found, Goal) then true else throw({Found, Goal}) )
      else throw({"failed", Goal})
    ),
    true.

%-----------------------------------------------------------------------------%
:- end_module arithmetic.test.
%-----------------------------------------------------------------------------%

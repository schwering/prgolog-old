%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.fuzzy.m.
% Main author: schwering.
%
% Fuzzy layer ontop of the RSTC.
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.fuzzy.

:- interface.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type category
    % NTG
    --->    very_far_behind ; far_behind ; behind ; close_behind
    ;       very_close_behind ; side_by_side ; very_close_infront
    ;       close_infront ; infront ; far_infront ; very_far_infront
    % TTC
    ;       contracting_fast ; contracting ; contracting_slowly ; reached
    ;       expanding_slowly ; expanding ; expanding_fast.

:- inst ntg
    --->    very_far_behind ; far_behind ; behind ; close_behind
    ;       very_close_behind ; side_by_side ; very_close_infront
    ;       close_infront ; infront ; far_infront ; very_far_infront.

:- inst ttc
    --->    contracting_slowly ; contracting ; contracting_fast ; reached
    ;       expanding_fast ; expanding ; expanding_slowly.

%-----------------------------------------------------------------------------%

:- pred A  `in` category <= arithmetic(A).
:- mode in `in` in  is semidet.
:- mode in `in` out is nondet.

:- pred A  `not_in` category <= arithmetic(A).
:- mode in `not_in` in  is semidet.
:- mode in `not_in` out is nondet.

:- pred (A::in) `in_all`  (list(category)::in) is semidet <= arithmetic(A).
:- pred (A::in) `in_any`  (list(category)::in) is semidet <= arithmetic(A).
:- pred (A::in) `in_none` (list(category)::in) is semidet <= arithmetic(A).

:- func defuzzify(category) = A <= arithmetic(A).

%-----------------------------------------------------------------------------%



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- type memdeg == float.
:- type mu(A) ---> triangle(A, A, A)
              ;    left_border(A, A)
              ;    right_border(A, A).

%-----------------------------------------------------------------------------%

:- func eval_triangle(mu(A), A) = memdeg <= arithmetic(A).

eval_triangle(left_border(L, R), Val) = to_float(Mu) :-
    (        if Val < L then  Mu = one
        else if Val > R then  Mu = zero
        else                  Mu = one - (Val - L) / (R - L)
    ).
eval_triangle(triangle(L, P, R), Val) = to_float(Mu) :-
    (        if Val < L then  Mu = zero
        else if Val > R then  Mu = zero
        else if Val < P then  Mu = (Val - L) / (P - L)
        else                  Mu = one - (Val - P) / (R - P)
    ).
eval_triangle(right_border(L, R), Val) = to_float(Mu) :-
    (        if Val < L then  Mu = zero
        else if Val > R then  Mu = one
        else                  Mu = (Val - L) / (R - L)
    ).


:- func mk_left_border(float, float) = mu(A) <= arithmetic(A).
:- func mk_triangle(float, float, float) = mu(A) <= arithmetic(A).
:- func mk_right_border(float, float) = mu(A) <= arithmetic(A).

mk_left_border(P, R) = left_border(from_float(P), from_float(R)).
mk_triangle(L, P, R) = triangle(from_float(L), from_float(P), from_float(R)).
mk_right_border(P, R) = right_border(from_float(P), from_float(R)).


:- func mu(category) = mu(A) <= arithmetic(A).
:- mode mu(in) = out is det.
:- mode mu(out) = out is multi.

mu(very_far_infront)   =  mk_left_border(         -9.0,   -5.0 ).
mu(far_infront)        =     mk_triangle( -7.0,   -5.0,   -3.0 ).
mu(infront)            =     mk_triangle( -4.0,   -3.0,   -2.0 ).
mu(close_infront)      =     mk_triangle( -2.5,   -2.0,   -1.0 ).
mu(very_close_infront) =     mk_triangle( -1.5,   -1.0,   -0.5 ).
mu(side_by_side)       =     mk_triangle( -0.75,   0.0,    0.75).
mu(very_close_behind)  =     mk_triangle(  0.5,    1.0,    1.5 ).
mu(close_behind)       =     mk_triangle(  1.0,    2.0,    2.5 ).
mu(behind)             =     mk_triangle(  2.0,    3.0,    4.0 ).
mu(far_behind)         =     mk_triangle(  3.0,    5.0,    7.0 ).
mu(very_far_behind)    = mk_right_border(  5.0,    9.0         ).

mu(expanding_slowly)   =  mk_left_border(        -15.0,  -10.0 ).
mu(expanding)          =     mk_triangle(-12.0,   -7.0,   -3.5 ).
mu(expanding_fast)     =     mk_triangle( -5.0,   -2.5,    0.0 ).
mu(reached)            =     mk_triangle( -2.0,    0.0,    2.0 ).
mu(contracting_fast)   =     mk_triangle(  0.0,    2.5,    5.0 ).
mu(contracting)        =     mk_triangle(  3.5,    7.0,   12.0 ).
mu(contracting_slowly) = mk_right_border(  10.0,  15.0         ).


:- func mu(category, A) = memdeg <= arithmetic(A).
:- mode mu(in, in) = out is det.
:- mode mu(out, in) = out is multi.

mu(Cat, Val) = eval_triangle(mu(Cat), Val).

%-----------------------------------------------------------------------------%

:- func combine(func(memdeg, memdeg) = memdeg, A, list(category), memdeg) =
    memdeg <= arithmetic(A).

combine(CombineF, Val, Cats, Initial) =
    foldl(func(Cat, Mem) = CombineF(mu(Cat, Val), Mem), Cats, Initial).


Val `in` Cat :- mu(Cat, Val) `float.'>'` 0.0.
Val `not_in` Cat :- mu(Cat, Val) = 0.0.
Val `in_all` Cats :- combine(float.min, Val, Cats, 1.0) `float.'>'` 0.0.
Val `in_any` Cats :- combine(float.max, Val, Cats, 0.0) `float.'>'` 0.0.
Val `in_none` Cats :- combine(float.max, Val, Cats, 0.0) = 0.0.


defuzzify(Cat) = Val :-
    mu(Cat) = Mu,
    (   Mu = left_border(Val, _)
    ;   Mu = triangle(_, Val, _)
    ;   Mu = right_border(_, Val)
    ).

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.fuzzy.
%-----------------------------------------------------------------------------%

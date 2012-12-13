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

:- pred ntg_cat(category).
:- mode ntg_cat(out(ntg)) is multi.
:- mode ntg_cat(in(ntg)) is det.
:- mode ntg_cat(in(ttc)) is failure.

:- pred ttc_cat(category).
:- mode ttc_cat(out(ttc)) is multi.
:- mode ttc_cat(in(ttc)) is det.
:- mode ttc_cat(in(ntg)) is failure.

%-----------------------------------------------------------------------------%

:- pred num(N) `in` category <= arithmetic.arithmetic(N).
:- mode in `in` in  is semidet.
:- mode in `in` out is nondet.

:- pred num(N) `not_in` category <= arithmetic.arithmetic(N).
:- mode in `not_in` in  is semidet.
:- mode in `not_in` out is nondet.

:- pred num(N) `in_all` list(category) <= arithmetic.arithmetic(N).
:- mode in `in_all` in is semidet.

:- pred num(N) `in_any` list(category) <= arithmetic.arithmetic(N).
:- mode in `in_any` in is semidet.

:- pred num(N) `in_none` list(category) <= arithmetic.arithmetic(N).
:- mode in `in_none` in is semidet.

:- func defuzzify(category) = num(N) <= arithmetic.arithmetic(N).
:- mode defuzzify(in) = out(basic_num) is det.

%-----------------------------------------------------------------------------%

:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module exception.

%-----------------------------------------------------------------------------%

:- type memdeg == float.
:- type mu(N) ---> triangle(num(N), num(N), num(N))
              ;    left_border(num(N), num(N))
              ;    right_border(num(N), num(N)).
:- inst basic_mu ---> triangle(basic_num, basic_num, basic_num)
                 ;    left_border(basic_num, basic_num)
                 ;    right_border(basic_num, basic_num).

%-----------------------------------------------------------------------------%

ntg_cat(very_far_infront).
ntg_cat(far_infront).
ntg_cat(infront).
ntg_cat(close_infront).
ntg_cat(very_close_infront).
ntg_cat(side_by_side).
ntg_cat(very_close_behind).
ntg_cat(close_behind).
ntg_cat(behind).
ntg_cat(far_behind).
ntg_cat(very_far_behind).

ttc_cat(expanding_slowly).
ttc_cat(expanding).
ttc_cat(expanding_fast).
ttc_cat(reached).
ttc_cat(contracting_fast).
ttc_cat(contracting).
ttc_cat(contracting_slowly).

%-----------------------------------------------------------------------------%

:- func det_lin_f(num(N), num(N), num(N)) = num(N) <= arithmetic.arithmetic(N).

det_lin_f(Lo, Hi, X) = Y :-
    if      Y1 = (X - Lo) / (Hi - Lo)
    then    Y = Y1
    else    throw({"Linear function", Lo, Hi, X}).


:- func eval_triangle(mu(N), num(N)) = memdeg <= arithmetic.arithmetic(N).
:- mode eval_triangle(in(basic_mu), in) = out is det.
:- mode eval_triangle(in, in) = out is det.

eval_triangle(left_border(L, R), V) = arithmetic.to_float(det_basic(Mu)) :-
         if V < L then Mu = one
    else if V > R then Mu = zero
    else               Mu = one - det_lin_f(L, R, V).
eval_triangle(triangle(L, P, R), V) = arithmetic.to_float(det_basic(Mu)) :-
         if V < L then Mu = zero
    else if V > R then Mu = zero
    else if V < P then Mu = det_lin_f(L, P, V)
    else               Mu = one - det_lin_f(P, R, V).
eval_triangle(right_border(L, R), V) = arithmetic.to_float(det_basic(Mu)) :-
         if V < L then Mu = zero
    else if V > R then Mu = one
    else               Mu = det_lin_f(L, R, V).


:- func mk_left_border(float, float) = mu(N) <= arithmetic.arithmetic(N).
:- func mk_triangle(float, float, float) = mu(N) <= arithmetic.arithmetic(N).
:- func mk_right_border(float, float) = mu(N) <= arithmetic.arithmetic(N).

mk_left_border(P, R) = left_border(number_from_float(P),
                                   number_from_float(R)).
mk_triangle(L, P, R) = triangle(number_from_float(L),
                                number_from_float(P),
                                number_from_float(R)).
mk_right_border(P, R) = right_border(number_from_float(P),
                                     number_from_float(R)).


:- func mu(category) = mu(N) <= arithmetic.arithmetic(N).
:- mode mu(in) = out(basic_mu) is det.
:- mode mu(out) = out(basic_mu) is multi.

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


:- func mu(category, num(N)) = memdeg <= arithmetic.arithmetic(N).
:- mode mu(in, in) = out is det.
:- mode mu(out, in) = out is multi.

mu(Cat, Val) = eval_triangle(mu(Cat), Val).

%-----------------------------------------------------------------------------%

:- func combine(func(memdeg, memdeg) = memdeg, num(N),
                list(category), memdeg) = memdeg <= arithmetic.arithmetic(N).

combine(CombineF, Val, Cats, Initial) =
    foldl(func(Cat, Mem) = CombineF(mu(Cat, Val), Mem), Cats, Initial).


Val `in`      Cat  :- mu(Cat, Val) `float.'>'` 0.0.
Val `not_in`  Cat  :- mu(Cat, Val) = 0.0.
Val `in_all`  Cats :- combine(float.min, Val, Cats, 1.0) `float.'>'` 0.0.
Val `in_any`  Cats :- combine(float.max, Val, Cats, 0.0) `float.'>'` 0.0.
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

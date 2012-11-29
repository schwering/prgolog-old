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

:- pred N  `in` category <= arithmetic(N).
:- mode in `in` in  is semidet.
:- mode in `in` out is nondet.

:- pred N  `not_in` category <= arithmetic(N).
:- mode in `not_in` in  is semidet.
:- mode in `not_in` out is nondet.

:- pred (N::in) `in_all`  (list(category)::in) is semidet <= arithmetic(N).
:- pred (N::in) `in_any`  (list(category)::in) is semidet <= arithmetic(N).
:- pred (N::in) `in_none` (list(category)::in) is semidet <= arithmetic(N).

:- func defuzzify(category) = N <= arithmetic(N).

%-----------------------------------------------------------------------------%

:- func follow(agent, agent) `with_type` rstc.proc(N) <= arithmetic(N).
:- func overtake(agent, agent) `with_type` rstc.proc(N) <= arithmetic(N).

%-----------------------------------------------------------------------------%

:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- type memdeg == float.
:- type mu(N) ---> triangle(N, N, N)
              ;    left_border(N, N)
              ;    right_border(N, N).

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

:- func eval_triangle(mu(N), N) = memdeg <= arithmetic(N).

eval_triangle(left_border(L, R), Val) = to_float(Mu) :-
         if Val < L then  Mu = one
    else if Val > R then  Mu = zero
    else                  Mu = one - (Val - L) / (R - L).
eval_triangle(triangle(L, P, R), Val) = to_float(Mu) :-
         if Val < L then  Mu = zero
    else if Val > R then  Mu = zero
    else if Val < P then  Mu = (Val - L) / (P - L)
    else                  Mu = one - (Val - P) / (R - P).
eval_triangle(right_border(L, R), Val) = to_float(Mu) :-
         if Val < L then  Mu = zero
    else if Val > R then  Mu = one
    else                  Mu = (Val - L) / (R - L).


:- func mk_left_border(float, float) = mu(N) <= arithmetic(N).
:- func mk_triangle(float, float, float) = mu(N) <= arithmetic(N).
:- func mk_right_border(float, float) = mu(N) <= arithmetic(N).

mk_left_border(P, R) = left_border(from_float(P), from_float(R)).
mk_triangle(L, P, R) = triangle(from_float(L), from_float(P), from_float(R)).
mk_right_border(P, R) = right_border(from_float(P), from_float(R)).


:- func mu(category) = mu(N) <= arithmetic(N).
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


:- func mu(category, N) = memdeg <= arithmetic(N).
:- mode mu(in, in) = out is det.
:- mode mu(out, in) = out is multi.

mu(Cat, Val) = eval_triangle(mu(Cat), Val).

%-----------------------------------------------------------------------------%

:- func combine(func(memdeg, memdeg) = memdeg, N, list(category), memdeg) =
    memdeg <= arithmetic(N).

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

:- func accelf(agent, func(rstc.sit(N)) = N) `with_type` primf(rstc.prim(N))
    <= arithmetic(N).
:- mode accelf(in, func(in) = out is det, in) = out is det.
:- mode accelf(in, func(in) = out is semidet, in) = out is det.

accelf(B, AccelF, S) = ( if Q = AccelF(S) then accel(B, Q) else abort ).


:- func lcf(agent) `with_type` primf(rstc.prim(N)) <= arithmetic(N).

lcf(B, S) = ( if lane(B, S) = left then lc(B, right) else lc(B, left) ).


:- func ntg_after(agent, agent, rstc.sit(N), s(N)) = ntg(N) <= arithmetic(N).
:- mode ntg_after(in, in, in, in) = out is semidet.

ntg_after(B, C, S, T) = ntg(B, C, prgolog.do(wait(T), S)).


:- func ttc_after(agent, agent, rstc.sit(N), s(N)) = ttc(N) <= arithmetic(N).
:- mode ttc_after(in, in, in, in) = out is semidet.

ttc_after(B, C, S, T) = ttc(B, C, prgolog.do(wait(T), S)).


:- func max_search_time = N <= arithmetic(N).

max_search_time = two*two*two*two*two*two*two*two.


:- func await(func(rstc.sit(N)) = s(N), s(N)) `with_type` primf(rstc.prim(N))
    <= arithmetic(N).
:- mode await(in(func(in) = out is semidet), in, in) = out is det.

await(F, Goal, S) = A :-
    if   bin_search(func(T) = F(prgolog.do(wait(T), S)) is semidet,
                    zero, max_search_time, Goal, V1)
    then A = wait(V1)
    else A = abort.


follow(B, Victim) = P :-
    P = b(accelf(B, rel_v(Victim, B))).


overtake(B, Victim) = P :-
    P = b(accelf(B, rel_v(Victim, B))).

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.fuzzy.
%-----------------------------------------------------------------------------%

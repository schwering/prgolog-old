%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: tree.m.
% Main author: schwering.
%
% A tree container, generally similar to a list except that it also allows
% lazy entries and is not flat.
%
%-----------------------------------------------------------------------------%

:- module tree.

:- interface.

:- use_module list.

%-----------------------------------------------------------------------------%

:- type value_func(U, V) == (func(U) = V).
:- type next_func(U, V) == (func(U, value_func(U, V), comparison_func(V)) = U).

:- type tree(T, V)
    --->    empty
    ;       leaf(T)
    ;       some [U] lazy(next_func(U, V), U, func(U) = tree(T, V))
    ;       branch(tree(T, V), tree(T, V)).

:- inst strict(T)
    --->    empty
    ;       leaf(T)
    ;       branch(strict(T), strict(T)).

:- inst strict == strict(ground).

:- type force_args(T, V)
    --->    force_args(value_func(T, V), comparison_func(V), V).

%-----------------------------------------------------------------------------%

:- pred empty(tree(_, _)).
:- mode empty(in) is semidet.

:- pred singleton(tree(T, _), T).
:- mode singleton(in, out) is semidet.

%-----------------------------------------------------------------------------%

:- func force(force_args(T, V), tree(T, V)) = tree(T, V).
:- mode force(in, in) = out(strict) is det.

%-----------------------------------------------------------------------------%

:- func foldl(func(T, W) = W, tree(T, V), W) = W.
:- mode foldl(in, in(strict), in) = out is det.

:- func foldr(func(T, W) = W, tree(T, V), W) = W.
:- mode foldr(in, in(strict), in) = out is det.

%-----------------------------------------------------------------------------%

:- func map(func(T1) = T2, tree(T1, V)) = tree(T2, V).
:- mode map(func(in) = out is det,     in) = out is det.
:- mode map(func(in) = out is semidet, in) = out is det.

:- func mapt(func(T1) = tree(T2, V), tree(T1, V)) = tree(T2, V).
:- mode mapt(func(in) = out is det,     in) = out is det.
:- mode mapt(func(in) = out is semidet, in) = out is det.

%-----------------------------------------------------------------------------%

:- func reduce(func(T, T) = T, tree(T, V)) = T.
:- mode reduce(in, in(strict)) = out is semidet.

:- func map_reduce(func(T1) = T2, func(T2, T2) = T2, tree(T1, V)) = T2.
:- mode map_reduce(func(in) = out is det,     in, in(strict)) = out is semidet.
:- mode map_reduce(func(in) = out is semidet, in, in(strict)) = out is semidet.

%-----------------------------------------------------------------------------%

:- func tree_to_list(tree(T, _)) = list.list(T).
:- mode tree_to_list(in(strict)) = out is det.

%-----------------------------------------------------------------------------%

%:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

empty(empty).
empty(branch(T1, T2)) :- empty(T1), empty(T2).


singleton(leaf(X), X).
singleton(branch(T1, T2), X) :-
    promise_equivalent_solutions [X]
    (   empty(T1), singleton(T2, X)
    ;   singleton(T1, X), empty(T2)
    ).

%-----------------------------------------------------------------------------%

:- func fixpoint(func(T) = T, T) = T.
:- mode fixpoint(in, in) = out is det.

fixpoint(F, X0) = ( if X = X0 then X else fixpoint(F, X) ) :- X = F(X0).


:- func max(comparison_func(T), T, T) = T.

max(CmpF, X, Y) = ( if CmpF(X, Y) = (>) then X else Y ).


force(_, T @ empty) = T.
force(_, T @ leaf(_)) = T.
force(Args @ force_args(Val, Cmp, Min), lazy(Next, X0, G)) = T :-
    T = force(Args, G(fixpoint(F, X0))),
    F = (func(X) = Next(X, NewVal, Cmp)),
    Fold = (func(T1, V) = max(Cmp, Val(T1), V)),
    NewVal = (func(X) = foldl(Fold, force(Args, G(X)), Min)).
force(Args, branch(T1, T2)) = branch(force(Args, T1), force(Args, T2)).

%-----------------------------------------------------------------------------%

foldl(_, empty, E) = E.
foldl(F, leaf(X), E) = F(X, E).
foldl(F, branch(T1, T2), E) = foldl(F, T2, foldl(F, T1, E)).

foldr(_, empty, E) = E.
foldr(F, leaf(X), E) = F(X, E).
foldr(F, branch(T1, T2), E) = foldr(F, T1, foldr(F, T2, E)).

%-----------------------------------------------------------------------------%

map(_, empty) = empty.
map(F, leaf(X)) = ( if FX = F(X) then leaf(FX) else empty ).
map(F, lazy(Next, X0, T)) = 'new lazy'(Next, X0, func(U) = map(F, T(U))).
map(F, branch(T1, T2)) = branch(map(F, T1), map(F, T2)).


mapt(_, empty) = empty.
mapt(F, leaf(X)) = ( if FX = F(X) then FX else empty ).
mapt(F, lazy(Next, X0, T)) = 'new lazy'(Next, X0, func(U) = mapt(F, T(U))).
mapt(F, branch(T1, T2)) = branch(mapt(F, T1), mapt(F, T2)).

%-----------------------------------------------------------------------------%

reduce(_, leaf(X)) = X.
reduce(F, branch(T1, T2)) =
    (   if      reduce(F, T1) = X1
        then    (   if      reduce(F, T2) = X2
                    then    F(X1, X2)
                    else    X1
                )
        else    reduce(F, T2)
    ).


map_reduce(F, _, leaf(X)) = F(X).
map_reduce(F, G, branch(T1, T2)) =
    (   if      map_reduce(F, G, T1) = Y1
        then    (   if      map_reduce(F, G, T2) = Y2
                    then    G(Y1, Y2)
                    else    Y1
                )
        else    map_reduce(F, G, T2)
    ).

%-----------------------------------------------------------------------------%

tree_to_list(T) = foldr(list.cons, T, list.[]).

%-----------------------------------------------------------------------------%
:- end_module tree.
%-----------------------------------------------------------------------------%

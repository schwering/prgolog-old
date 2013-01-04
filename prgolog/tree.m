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

:- type tree(T, U)
    --->    empty
    ;       value(T)
    ;       lazy(func(U) = tree(T, U))
    ;       branch(tree(T, U), tree(T, U)).

:- inst strict(T)
    --->    empty
    ;       value(T)
    ;       branch(strict(T), strict(T)).

:- inst non_empty_strict(T)
    --->    value(T)
    ;       branch(non_empty_strict(T), non_empty_strict(T)).

:- inst strict == strict(ground).

:- inst non_empty_strict == non_empty_strict(ground).

:- inst lazy
    --->    empty
    ;       lazy(ground)
    ;       branch(lazy, lazy).

%-----------------------------------------------------------------------------%

:- pred empty(tree(_, _)).
:- mode empty(in) is semidet.

:- pred singleton(tree(T, _), T).
:- mode singleton(in, out) is semidet.

%-----------------------------------------------------------------------------%

:- func force(U, tree(T, U)) = tree(T, U).
:- mode force(in, in) = out(strict) is det.

%-----------------------------------------------------------------------------%

:- func foldl(func(T, V) = V, tree(T, _), V) = V.
:- mode foldl(in, in(strict), in) = out is det.

:- func foldr(func(T, V) = V, tree(T, _), V) = V.
:- mode foldr(in, in(strict), in) = out is det.

%-----------------------------------------------------------------------------%

:- func map(func(T1) = T2, tree(T1, U)) = tree(T2, U).
:- mode map(in, in(strict)) = out(strict) is det.
:- mode map(in, in(non_empty_strict)) = out(non_empty_strict) is det.
:- mode map(in, in(lazy)) = out(lazy) is det.
:- mode map(in, in) = out is det.

:- func mapt(func(T1) = tree(T2, U), tree(T1, U)) = tree(T2, U).
:- mode mapt(in, in) = out is det.

%-----------------------------------------------------------------------------%

:- func reduce(func(T, T) = T, tree(T, U)) = T.
:- mode reduce(in, in(strict)) = out is semidet.
:- mode reduce(in, in(non_empty_strict)) = out is det.

:- func map_reduce(func(T) = V, func(V, V) = V, tree(T, U)) = V.
:- mode map_reduce(in, in, in(strict)) = out is semidet.
:- mode map_reduce(in, in, in(non_empty_strict)) = out is det.

%-----------------------------------------------------------------------------%

:- func tree_to_list(tree(T, _)) = list.list(T).
:- mode tree_to_list(in(strict)) = out is det.
:- mode tree_to_list(in(non_empty_strict)) = out(list.non_empty_list) is det.

%-----------------------------------------------------------------------------%

%:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

empty(empty).
empty(branch(T1, T2)) :- empty(T1), empty(T2).


singleton(value(X), X).
singleton(branch(T1, T2), X) :-
    promise_equivalent_solutions [X]
    (   empty(T1), singleton(T2, X)
    ;   singleton(T1, X), empty(T2)
    ).

%-----------------------------------------------------------------------------%

force(_, T @ empty) = T.
force(_, T @ value(_)) = T.
force(U, lazy(F)) = force(U, F(U)).
force(U, branch(T1, T2)) = branch(force(U, T1), force(U, T2)).

%-----------------------------------------------------------------------------%

foldl(_, empty, E) = E.
foldl(F, value(X), E) = F(X, E).
foldl(F, branch(T1, T2), E) = foldl(F, T2, foldl(F, T1, E)).

foldr(_, empty, E) = E.
foldr(F, value(X), E) = F(X, E).
foldr(F, branch(T1, T2), E) = foldr(F, T1, foldr(F, T2, E)).

%-----------------------------------------------------------------------------%

map(_, empty) = empty.
map(F, value(X)) = value(F(X)).
map(F, lazy(G)) = lazy(func(U) = map(F, G(U))).
map(F, branch(T1, T2)) = branch(map(F, T1), map(F, T2)).


mapt(_, empty) = empty.
mapt(F, value(X)) = F(X).
mapt(F, lazy(G)) = lazy(func(U) = mapt(F, G(U))).
mapt(F, branch(T1, T2)) = branch(mapt(F, T1), mapt(F, T2)).

%-----------------------------------------------------------------------------%

reduce(_, value(X)) = X.
reduce(F, branch(T1, T2)) =
    (   if      reduce(F, T1) = X1
        then    (   if      reduce(F, T2) = X2
                    then    F(X1, X2)
                    else    X1
                )
        else    reduce(F, T2)
    ).

map_reduce(F, G, T) = reduce(G, map(F, T)).

%-----------------------------------------------------------------------------%

tree_to_list(T) = foldr(list.cons, T, list.[]).

%-----------------------------------------------------------------------------%
:- end_module tree.
%-----------------------------------------------------------------------------%

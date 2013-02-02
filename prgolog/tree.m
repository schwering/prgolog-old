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
% As a convention, we use T as type variable for the tree element types and
% V as type for the evaluation result. Further there is an existentially
% quantified U (see below).
%
% A lazy entry consists of a function G that maps some type U to a sub-tree.
% Furthermore it provides a opti_func Opti and an initial datum X0.
% The task of Opti is to search for the optimal datum X; the sub-tree G(X) is
% then hooked in the tree. X0 may serve as a starting point for the search
% of Opti.
% To give Opti a hand, it is provided with a value_func that evalutes the
% quality of potential solutions by returning some value of type U. These
% values can be compared using the likewise provided comparison_func function.
% Opti might, for example, maximize value_func.
%
%-----------------------------------------------------------------------------%

:- module tree.

:- interface.

:- use_module list.

%-----------------------------------------------------------------------------%

:- type value_func(U, V) == (func(U) = V).
:- type opti_func(U, V) == (func(U, value_func(U, V), comparison_func(V)) = U).

:- inst semidet_value_func == (func(in) = out is semidet).

:- type tree(T, V)
    --->    empty
    ;       leaf(T)
    ;       lazy((func) = tree(T, V))
    ;       some [U] sprout(opti_func(U, V), U, func(U) = tree(T, V))
    ;       branch(tree(T, V), tree(T, V)).

:- inst adult(T)
    --->    empty
    ;       leaf(T)
    ;       lazy((func) = out(adult(T)) is det)
    ;       branch(adult(T), adult(T)).

:- inst adult == adult(ground).

:- type force_args(T, V)
    --->    force_args(value_func(T, V), comparison_func(V), V).

%-----------------------------------------------------------------------------%

:- func lazily(func(tree(T, V)) = tree(T, V), tree(T, V)) = tree(T, V).
:- mode lazily(in, in) = out is det.

%-----------------------------------------------------------------------------%

:- func force(force_args(T, V), tree(T, V)) = tree(T, V).
:- mode force(in, in) = out(adult) is det.

:- func load(force_args(T, V), tree(T, V)) = tree(T, V).
:- mode load(in, in) = out(adult) is det.

%-----------------------------------------------------------------------------%

:- func foldl(func(T, W) = W, tree(T, V), W) = W.
:- mode foldl(in, in(adult), in) = out is det.

:- func foldr(func(T, W) = W, tree(T, V), W) = W.
:- mode foldr(in, in(adult), in) = out is det.

%-----------------------------------------------------------------------------%

:- func map(func(T1) = T2, tree(T1, V)) = tree(T2, V).
:- mode map(func(in) = out is det,     in) = out is det.
:- mode map(func(in) = out is semidet, in) = out is det.

:- func mapt(func(T1) = tree(T2, V), tree(T1, V)) = tree(T2, V).
:- mode mapt(func(in) = out is det,     in) = out is det.
:- mode mapt(func(in) = out is semidet, in) = out is det.

%-----------------------------------------------------------------------------%

:- func reduce(func(T, T) = T, tree(T, V)) = T.
:- mode reduce(in, in(adult)) = out is semidet.

:- func reduce(func(T, T) = T, tree(T, V), T) = T.
:- mode reduce(in, in(adult), in) = out is det.

:- func map_reduce(func(T1) = T2, func(T2, T2) = T2, tree(T1, V)) = T2.
:- mode map_reduce(func(in) = out is det,     in, in(adult)) = out is semidet.
:- mode map_reduce(func(in) = out is semidet, in, in(adult)) = out is semidet.

:- func map_reduce(func(T1) = T2, func(T2, T2) = T2, tree(T1, V), T2) = T2.
:- mode map_reduce(func(in) = out is det,     in, in(adult), in) = out is det.
:- mode map_reduce(func(in) = out is semidet, in, in(adult), in) = out is det.

%:- type commit(T) ---> commit(T) ; continue(T).
%
%:- func commit_map_reduce(func(T1) = commit(T2), func(T2, T2) = T2, tree(T1, V)) = commit(T2).
%:- mode commit_map_reduce(func(in) = out is det,     in, in(adult)) = out is semidet.
%:- mode commit_map_reduce(func(in) = out is semidet, in, in(adult)) = out is semidet.

%-----------------------------------------------------------------------------%

:- pred max_search(comparison_func(V),
                   value_func(T, V),
                   value_func(T, V),
                   force_args(T, V),
                   tree(T, V),
                   V, T).
:- mode max_search(in, in, in, in, in, out, out) is semidet.
:- mode max_search(in, in, in(semidet_value_func), in, in, out, out) is semidet.
:- mode max_search(in, in(semidet_value_func), in(semidet_value_func),
                   in, in, out, out) is semidet.

%-----------------------------------------------------------------------------%

:- func tree_to_list(tree(T, _)) = list.list(T).
:- mode tree_to_list(in(adult)) = out is det.

%-----------------------------------------------------------------------------%

:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module maybe.

%-----------------------------------------------------------------------------%

lazily(F, T @ empty) = F(T). %lazy((func) = F(T)).
lazily(F, T @ leaf(_)) = F(T). %lazy((func) = F(T)).
lazily(F, lazy(T)) = lazy((func) = F(apply(T))).
lazily(F, T @ sprout(_, _, _)) = lazy((func) = lazily(F, F(T))).
lazily(F, branch(T1, T2)) = lazy((func) = branch(lazily(F, T1), lazily(F, T2))).

%-----------------------------------------------------------------------------%

:- func max(comparison_func(T), T, T) = T.

max(CmpF, X, Y) = ( if CmpF(X, Y) = (>) then X else Y ).


force(_, empty) = empty.
force(_, leaf(X)) = leaf(X).
force(Args, lazy(T)) = force(Args, apply(T)).
force(Args @ force_args(Val, Cmp, Min), sprout(Opti, X0, G)) = T :-
    T = force(Args, G(Opti(X0, Val1, Cmp))),
    Val1 = (func(X) = map_reduce(Val, max(Cmp), force(Args, G(X)), Min)).
force(Args, branch(T1, T2)) = branch(force(Args, T1), force(Args, T2)).


load(Args, T) = lazily(lazily(force(Args)), T).

% XXX remove if it's really equivalent to the previous definition
%load(_, empty) = empty.
%load(_, leaf(X)) = leaf(X).
%load(Args, lazy(T)) = lazy((func) = load(Args, apply(T))).
%load(Args @ force_args(Val, Cmp, Min), sprout(Opti, X0, G)) = lazy(T) :-
%    T = ((func) = load(Args, G(Opti(X0, Val1, Cmp)))),
%    Val1 = (func(X) = map_reduce(Val, max(Cmp), load(Args, G(X)), Min)).
%load(Args, branch(T1, T2)) = branch(load(Args, T1), load(Args, T2)).

%-----------------------------------------------------------------------------%

foldl(_, empty, E) = E.
foldl(F, leaf(X), E) = F(X, E).
foldl(F, lazy(T), E) = foldl(F, apply(T), E).
foldl(F, branch(T1, T2), E) = foldl(F, T2, foldl(F, T1, E)).

foldr(_, empty, E) = E.
foldr(F, leaf(X), E) = F(X, E).
foldr(F, lazy(T), E) = foldr(F, apply(T), E).
foldr(F, branch(T1, T2), E) = foldr(F, T1, foldr(F, T2, E)).

%-----------------------------------------------------------------------------%

map(_, empty) = empty.
map(F, leaf(X)) = ( if FX = F(X) then leaf(FX) else empty ).
map(F, lazy(T)) = lazy((func) = map(F, apply(T))).
map(F, sprout(Opti, X0, T)) = 'new sprout'(Opti, X0, func(U) = map(F, T(U))).
map(F, branch(T1, T2)) = branch(map(F, T1), map(F, T2)).


mapt(_, empty) = empty.
mapt(F, leaf(X)) = ( if FX = F(X) then FX else empty ).
mapt(F, lazy(T)) = lazy((func) = mapt(F, apply(T))).
mapt(F, sprout(Opti, X0, T)) = 'new sprout'(Opti, X0, func(U) = mapt(F, T(U))).
mapt(F, branch(T1, T2)) = branch(mapt(F, T1), mapt(F, T2)).

%-----------------------------------------------------------------------------%

reduce(_, leaf(X)) = X.
reduce(F, lazy(T)) = reduce(F, apply(T)).
reduce(F, branch(T1, T2)) =
    (   if      reduce(F, T1) = X1
        then    (   if      reduce(F, T2) = X2
                    then    F(X1, X2)
                    else    X1
                )
        else    reduce(F, T2)
    ).


map_reduce(F, _, leaf(X)) = F(X).
map_reduce(F, G, lazy(T)) = map_reduce(F, G, apply(T)).
map_reduce(F, G, branch(T1, T2)) =
    (   if      map_reduce(F, G, T1) = Y1
        then    (   if      map_reduce(F, G, T2) = Y2
                    then    G(Y1, Y2)
                    else    Y1
                )
        else    map_reduce(F, G, T2)
    ).


reduce(F, T, Def) = ( if X = reduce(F, T) then X else Def ).


map_reduce(F, G, T, Def) = ( if X = map_reduce(F, G, T) then X else Def ).


%commit_map_reduce(F, _, leaf(X)) = F(X).
%commit_map_reduce(F, G, lazy(T)) = commit_map_reduce(F, G, apply(T)).
%commit_map_reduce(F, G, branch(T1, T2)) = Y :-
%    (   if      commit_map_reduce(F, G, T1) = Y1
%        then    (   Y1 = commit(_),
%                    Y = Y1
%                ;   Y1 = continue(V1),
%                    (   if      commit_map_reduce(F, G, T2) = Y2
%                        then    (   Y2 = commit(_),
%                                    Y = Y2
%                                ;   Y2 = continue(V2),
%                                    Y = continue(G(V1, V2))
%                                )
%                        else    Y = Y1
%                    )
%                )
%        else    Y = commit_map_reduce(F, G, T2)
%    ).

%-----------------------------------------------------------------------------%

:- type value_tree(T, V) == tree({V, tree(T, V)}, V).
:- type heu_value_tree(T, V) == tree({V, value_tree(T, V)}, V).

:- type iter_pred(T, V, W) == (pred(W, W, V, tree(T, V))).
:- inst iter_pred == (pred(in, out, out, out) is semidet).

%-----------------------------------------------------------------------------%

:- func value(comparison_func(V), value_func(T, V),
              tree(T, V)) = value_tree(T, V).
:- mode value(in, in, in) = out(adult) is det.
:- mode value(in, in(semidet_value_func), in) = out(adult) is det.

value(_, _, empty) = empty.
value(_, Val, T @ leaf(X)) = ( if V = Val(X) then leaf({V, T}) else empty ).
value(Cmp, Val, lazy(T)) = value(Cmp, Val, apply(T)).
value(Cmp, Val, T @ sprout(_, X0, G)) =
    ( if V = reduce(max(Cmp), map(Val, G(X0))) then leaf({V, T}) else empty ).
value(Cmp, Val, branch(T1, T2)) =
    branch(value(Cmp, Val, T1), value(Cmp, Val, T2)).


:- pred extract_max(comparison_func(V)::in,
                    value_tree(T, V)::in(adult),
                    value_tree(T, V)::out(adult),
                    V::out, tree(T, V)::out) is semidet.

extract_max(_, leaf({V, T}), empty, V, T).
extract_max(Cmp, lazy(T), R, V, M) :-
    extract_max(Cmp, apply(T), R, V, M).
extract_max(Cmp, branch(T1, T2), R, V, M) :-
    if      extract_max(Cmp, T1, R1, V1, M1)
    then    (   if      extract_max(Cmp, T2, R2, V2, M2)
                then    (   if      Cmp(V2, V1) = (>)
                            then    R = branch(T1, R2), V = V2, M = M2
                            else    R = branch(R1, T2), V = V1, M = M1
                        )
                else    R = R1, V = V1, M = M1
            )
    else    extract_max(Cmp, T2, R, V, M).


:- pred first(comparison_func(V)::in,
              % The old and new valued tree.
              heu_value_tree(T, V)::in, heu_value_tree(T, V)::out,
              % The maximum value and the corresponding tree.
              V::out, tree(T, V)::out) is semidet.

first(Cmp, HVT0, HVT1, MaxV, MaxT) :-
    extract_max(Cmp, HVT0, HVT1, _, MaxVT),
    extract_max(Cmp, MaxVT, _, MaxV, MaxT).


:- pred next(comparison_func(V)::in,
             % The old and new valued tree.
             heu_value_tree(T, V)::in, heu_value_tree(T, V)::out,
             % The old and new maximum value.
             V::in, V::out,
             % The tree with the maximum value.
             tree(T, V)::out) is semidet.

next(Cmp, !HVT, MaxV0, MaxV1, MaxT) :-
    extract_max(Cmp, !HVT, H, VT),
    Cmp(H, MaxV0) \= (<), % next tree might be better than the last one
    extract_max(Cmp, VT, _, V, T),
    (   if      Cmp(V, MaxV0) \= (<) % next tree is indeed better
        then    MaxV1 = V, MaxT = T
        else    next(Cmp, !HVT, MaxV0, MaxV1, MaxT)
    ).


:- some [W] pred max_iter(comparison_func(V),
                          value_func(T, V),
                          value_func(T, V),
                          force_args(T, V),
                          tree(T, V), W, iter_pred(T, V, W)).
:- mode max_iter(in, in, in,
                 in, in, out, out(iter_pred)) is det.
:- mode max_iter(in, in, in(semidet_value_func),
                 in, in, out, out(iter_pred)) is det.
:- mode max_iter(in, in(semidet_value_func), in(semidet_value_func),
                 in, in, out, out(iter_pred)) is det.

max_iter(Cmp, Heu, Val, Args, T, {HVT, no}, Next) :-
    HVT = map(func({H, T1}) = {H, value(Cmp, Val, load(Args, T1))},
              value(Cmp, Heu, T)),
    Next = (pred({HVT0, MV0}::in, {HVT1, yes(V1)}::out,
                 V1::out, T1::out) is semidet :-
        (   MV0 = no, first(Cmp, HVT0, HVT1, V1, T1)
        ;   MV0 = yes(V0), next(Cmp, HVT0, HVT1, V0, V1, T1)
        )
    ).


:- pred iter_end(iter_pred(T, V, W), W, W, V, tree(T, V)).
:- mode iter_end(in(iter_pred), in, out, out, out) is semidet.

iter_end(Next, !Iter, V, T) :-
    Next(!Iter, V0, T0),
    (   if      iter_end(Next, !Iter, V1, T1)
        then    V = V1, T = T1
        else    V = V0, T = T0
    ).


max_search(Cmp, Heu, Val, Args, T, V, X) :-
    max_iter(Cmp, Heu, Val, Args, T, Iter, Next),
    iter_end(Next, Iter, _, V, leaf(X)).

%-----------------------------------------------------------------------------%

tree_to_list(T) = foldr(list.cons, T, list.[]).

%-----------------------------------------------------------------------------%
:- end_module tree.
%-----------------------------------------------------------------------------%

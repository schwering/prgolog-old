%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: tree.test.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module tree.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test_force(io::di, io::uo) is det.
:- pred test_search_det(io::di, io::uo) is det.
:- pred test_search_semidet(io::di, io::uo) is det.
:- pred test_search2(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module require.
:- import_module std_util.

%-----------------------------------------------------------------------------%

test_force(!IO) :-
    T = branch(branch(leaf(1), leaf(2)), branch(leaf(3), leaf(4))) : tree(int, int),
    Args = force_args(id, ordering, 0),
    format("%s\n", [s(string(T))], !IO),
    F = map((func(N) = N + 1 is det :- trace [io(!SubIO)] ( format("traverse(%d)\n", [i(N)], !SubIO) ))),
    T1 = F(T),
    format("T1: %s\n", [s(string(T1))], !IO),
    T2 = lazily(F, T1),
    format("T2: %s\n", [s(string(T2))], !IO),
    T3 = force(Args, T2),
    format("T3: %s\n", [s(string(T3))], !IO),
    T4 = load(Args, T2),
    format("T4: %s\n", [s(string(T4))], !IO),
    T5 = force(Args, T4),
    format("T5: %s\n", [s(string(T4))], !IO),
    true.


test_search_det(!IO) :-
    H = (func(X) = ( if X mod 2 = 0 then X else 100 )),
    V = (func(X) = X :- ( if X = 0 then throw({"shouldn't expand this node", X}) else true )),
    Cmp = ordering,
    Args = force_args(V, Cmp, 0),
    Data = [
        {   empty
        ,
            [ ]
        },
        {   branch(
                branch(
                    leaf(3),
                    leaf(4)
                ),
                branch(
                    leaf(5),
                    leaf(6)
                )
            )
        ,
            [ leaf(3), leaf(5), leaf(6) ]
        },
        {   branch(
                branch(
                    leaf(4),
                    leaf(5)
                ),
                branch(
                    leaf(3),
                    leaf(6)
                )
            )
        ,
            [ leaf(5), leaf(6) ]
        },
        {   branch(
                branch(
                    leaf(5),
                    leaf(5)
                ),
                branch(
                    leaf(5),
                    leaf(6)
                )
            )
        ,
            [ leaf(5), leaf(5), leaf(5), leaf(6) ]
        },
        {   branch(
                branch(
                    leaf(2),
                    lazy((func) = leaf(4))
                ),
                branch(
                    leaf(5),
                    lazy((func) = leaf(6))
                )
            )
        ,
            [ leaf(5), leaf(6) ]
        },
        {   branch(
                branch(
                    leaf(4),
                    lazy((func) = leaf(4))
                ),
                branch(
                    leaf(4),
                    lazy((func) = leaf(4))
                )
            )
        ,
            [ leaf(4), leaf(4), leaf(4), leaf(4) ]
        },
        {   branch(
                branch(
                    leaf(2),
                    leaf(4)
                ),
                branch(
                    leaf(6),
                    'new sprout'(func(U, _, _) = U-2, 7, func(U) = leaf(U))
                )
            )
        ,
            [ leaf(5), leaf(6) ]
        },
        {   branch(
                branch(
                    leaf(2),
                    leaf(4)
                ),
                branch(
                    leaf(8),
                    'new sprout'(func(U, _, _) = U, 8, func(U) = leaf(U))
                )
            )
        ,
            [ leaf(8), leaf(8) ]
        },
        {   branch(
                branch(
                    leaf(2),
                    leaf(4)
                ),
                branch(
                    'new sprout'(func(U, _, _) = ( if U = 10 then 10 else throw({"sprout should not be optimized"}) ), 10, func(U) = leaf(U)),
                    'new sprout'(func(U, _, _) = U, 10, func(U) = leaf(U))
                )
            )
        ,
            [ leaf(10), leaf(10) ]
        },
        {   branch(
                branch(
                    leaf(3),
                    leaf(0)
                ),
                branch(
                    leaf(5),
                    leaf(6)
                )
            )
        ,
            [ leaf(3), leaf(5), leaf(6) ]
        }
    ],
    foldl((pred({T, Exp}::in, !.SubIO::di, !:SubIO::uo) is det :-
        max_iter(Cmp, Cmp, H, V, Args, T, Iterator, Next),
        foldl((pred(Y::in, !.Iter::in, !:Iter::out) is det :-
            ( if Next(!Iter, _, X) then ( if X = Y then true else throw({"found unexpected instead of", X, Y}) ) else throw({"found nothing instead of", Y}) )
        ), Exp, Iterator, EmptyIterator),
        ( if Next(EmptyIterator, _, _, Z) then throw({"found unexpected", Z}) else true )
    ), Data, !IO),
    true.


test_search_semidet(!IO) :-
    H = (func(X) = ( if X mod 2 = 0 then X else 100 )),
    V = (func(X) = X is semidet :- X mod 3 \= 0),
    Cmp = ordering,
    Args = force_args(V, Cmp, 0),
    Data = [
        {   empty
        ,
            [ ]
        },
        {   leaf(3)
        ,
            [ leaf(3) ] % for value_func_semidet, even X with failing V(X) may be returned
        },
        {   leaf(4)
        ,
            [ leaf(4) ]
        },
        {   branch(
                leaf(3),
                leaf(4)
            )
        ,
            [ leaf(3), leaf(4) ]
        },
        {   branch(
                leaf(6),
                leaf(4)
            )
        ,
            [ leaf(6), leaf(4) ]
        },
        {   branch(
                leaf(3),
                leaf(2)
            )
        ,
            [ leaf(3), leaf(2) ]
        },
        {   branch(
                leaf(6),
                leaf(2)
            )
        ,
            [ leaf(6), leaf(2) ]
        },
        {   branch(
                branch(
                    leaf(3),
                    leaf(4)
                ),
                branch(
                    leaf(5),
                    leaf(6)
                )
            )
        ,
            [ leaf(3), leaf(5) ]
        },
        {   branch(
                branch(
                    leaf(4),
                    leaf(5)
                ),
                branch(
                    leaf(3),
                    leaf(6)
                )
            )
        ,
            [ leaf(5) ]
        },
        {   branch(
                branch(
                    leaf(5),
                    leaf(5)
                ),
                branch(
                    leaf(5),
                    leaf(8)
                )
            )
        ,
            [ leaf(5), leaf(5), leaf(5), leaf(8) ]
        },
        {   branch(
                branch(
                    leaf(2),
                    lazy((func) = leaf(4))
                ),
                branch(
                    leaf(5),
                    lazy((func) = leaf(6))
                )
            )
        ,
            [ leaf(5) ]
        },
        {   branch(
                branch(
                    leaf(4),
                    lazy((func) = leaf(4))
                ),
                branch(
                    leaf(4),
                    lazy((func) = leaf(4))
                )
            )
        ,
            [ leaf(4), leaf(4), leaf(4), leaf(4) ]
        },
        {   branch(
                branch(
                    leaf(2),
                    leaf(4)
                ),
                branch(
                    leaf(8),
                    'new sprout'(func(U, _, _) = U-2, 9, func(U) = leaf(U))
                )
            )
        ,
            [ leaf(7), leaf(8) ]
        },
        {   branch(
                branch(
                    leaf(2),
                    leaf(4)
                ),
                branch(
                    leaf(8),
                    'new sprout'(func(U, _, _) = U, 8, func(U) = leaf(U))
                )
            )
        ,
            [ leaf(8), leaf(8) ]
        },
        {   branch(
                branch(
                    leaf(2),
                    leaf(4)
                ),
                branch(
                    'new sprout'(func(U, _, _) = ( if U = 10 then 10 else throw({"sprout should not be optimized"}) ), 10, func(U) = leaf(U)),
                    'new sprout'(func(U, _, _) = U, 10, func(U) = leaf(U))
                )
            )
        ,
            [ leaf(10), leaf(10) ]
        }
    ],
    foldl((pred({T, Exp}::in, !.SubIO::di, !:SubIO::uo) is det :-
        max_iter(Cmp, Cmp, H, V, Args, T, Iterator, Next),
        foldl((pred(Y::in, !.Iter::in, !:Iter::out) is det :-
            ( if Next(!Iter, _, X) then ( if X = Y then true else throw({"found unexpected instead of", X, Y}) ) else throw({"found nothing instead of", Y}) )
        ), Exp, Iterator, EmptyIterator),
        ( if Next(EmptyIterator, _, _, Z) then throw({"found unexpected", Z}) else true )
    ), Data, !IO),
    test_search2(!IO),
    true.


test_search2(!IO) :-
    H = (func(_) = {100.0, 100}),
    V = (func(X1 @ {V1, _}) = X1 is semidet :- V1 >= 0.0),
    Cmp = (func(X1 @ {V1, N1}, X2 @ {V2, N2}) = ( if V1 > V2 ; V1 = V2, N1 > N2 then (>) else if X1 = X2 then (=) else (<) )) : comparison_func({float, int}),
    Min = {-1.0 * float.max, int.min_int},
    Args = force_args(V, Cmp, Min),
    Data = [
        {   branch(
                branch(
                    L1 @ leaf({-0.01, 1}),
                    L2 @ leaf({0.0, 0})
                ),
                branch(
                    L3 @ leaf({-0.02, 2}),
                    L4 @ leaf({0.0, 1})
                )
            )
        ,
            [ L1, L2, L4 ]
        }
    ],
    foldl((pred({T, Exp}::in, !.SubIO::di, !:SubIO::uo) is det :-
        max_iter(Cmp, Cmp, H, V, Args, T, Iterator, Next),
        foldl((pred(Y::in, !.Iter::in, !:Iter::out) is det :-
            ( if Next(!Iter, _, X) then ( if X = Y then true else throw({"found unexpected instead of", X, Y}) ) else throw({"found nothing instead of", Y}) )
        ), Exp, Iterator, EmptyIterator),
        ( if Next(EmptyIterator, _, _, Z) then throw({"found unexpected", Z}) else true )
    ), Data, !IO),
    true.


%-----------------------------------------------------------------------------%
:- end_module tree.test.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: prgolog.test.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module prgolog.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test_next(io::di, io::uo) is det.
:- pred test_next2(io::di, io::uo) is det.
:- pred test_final(io::di, io::uo) is det.
:- pred test_value(io::di, io::uo) is det.
:- pred test_trans_atom(io::di, io::uo) is det.
:- pred test_trans(io::di, io::uo) is det.
:- pred test_final2(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module list.
:- import_module prgolog.nice.
:- import_module string.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type prim ---> a ; b ; c ; d ; e ; e2 ; impossible
                ; set_val(string, float) ; inc_reward(reward).

:- instance bat(prim) where [
    poss(A, _) :- A \= impossible,
    reward(s0) = 0.0,
    reward(do(A, S)) = R + reward(S) :-
        (   A = a, R = 0.0
        ;   A = b, R = 1.0
        ;   A = c, R = 2.0
        ;   A = d, R = 3.0
        ;   A = e, R = 4.0
        ;   A = e2, R = 4.0
        ;   A = impossible, R = 0.0
        ;   A = set_val(_, _), R = 0.0
        ;   A = inc_reward(R)
        ),
    lookahead(_) = 3,
    new_lookahead(L, _) = L - 1
].


:- func get_val(string, sit(prim)) = float.
:- mode get_val(in, in) = out is semidet.

get_val(Key, do(A, S)) =
    ( if A = set_val(Key, Val) then Val else get_val(Key, S) ).


:- func get_val(string, float, sit(prim)) = float.
:- mode get_val(in, in, in) = out is det.

get_val(_, Default, s0) = Default.
get_val(Key, Default, do(A, S)) =
    ( if A = set_val(Key, Val) then Val else get_val(Key, Default, S) ).

%-----------------------------------------------------------------------------%

:- func last_action(sit(prim)) = prim.
:- mode last_action(in) = out is semidet.

last_action(do(A, _)) = A.

%-----------------------------------------------------------------------------%

:- func pseudo_pickbest(sit(A)) =
    tree.force_args(pseudo_decomp(A), value) <= bat(A).

pseudo_pickbest(S) = tree.force_args(Val, Cmp, Min) :-
    Val = (func(pseudo_decomp(C, R)) = value(seq(pseudo_atom(C), R), S)),
    Cmp = (func(V, W) = ( if V > W then (>) else if V = W then (=) else (<) )),
    Min = {min, min_int}.


test_next(!IO) :-
    Decomp = (func(Prim, Prog) = pseudo_decomp(atom(prim(Prim)), Prog)),
    Inputs =
    [   { nil,
        [] }
    ,   { a(a),
        [Decomp(a, nil)] }
    ,   { nil `;` a(a),
        [Decomp(a, nil)] }
    ,   { a(a) or a(b),
        [Decomp(a, nil), Decomp(b, nil)] }
    ,   { a(a) or a(b) or a(c),
        [Decomp(a, nil), Decomp(b, nil), Decomp(c, nil)] }
    ,   { a(a) // a(b),
        [Decomp(b, a(a) // nil), Decomp(a, nil // a(b))] }
    ,   { (a(a) `;` a(b)) // (a(c) `;` a(d)),
        [Decomp(c, (a(a) `;` a(b)) // (nil `;` a(d))),
         Decomp(a, (nil `;` a(b)) // (a(c) `;` a(d)))] }
    ,   { star(a(a) `;` a(b)),
        [Decomp(a, (nil `;` a(b)) `;` star(a(a) `;` a(b)))] }
    ] `with_type` list({prog(prim), _}),
    Test = (pred(P::in, Exp::in, Succ::out) is det :-
        T = next(P),
        S = s0,
        Ds = tree.tree_to_list(tree.force(pseudo_pickbest(S), T)),
        sort(Ds, Ds1),
        sort(Exp, Exp1),
        ( if Ds1 = Exp1 then Succ = yes else throw({Ds, Exp}) )
    ),
    list.foldl((func({P, Exp}, M) = N :-
        Test(P, Exp, Succ),
        N = M + ( if Succ = yes then 1 else 0 )
    ), Inputs, 0) = _Successes,
    %format("%d successes\n", [i(Successes)], !IO),
    true.

%-----------------------------------------------------------------------------%

test_next2(!IO) :-
    Decomp = (func(Prim, Prog) = decomp(prim(Prim), Prog)),
    Inputs =
    [   { nil,
        [] }
    ,   { a(a),
        [Decomp(a, nil)] }
    ,   { nil `;` a(a),
        [Decomp(a, nil)] }
    ,   { a(a) or a(b),
        [Decomp(a, nil), Decomp(b, nil)] }
    ,   { a(a) or a(b) or a(c),
        [Decomp(a, nil), Decomp(b, nil), Decomp(c, nil)] }
    ,   { a(a) // a(b),
        [Decomp(a, nil // a(b)), Decomp(b, a(a) // nil)] }
    ,   { (a(a) `;` a(b)) // (a(c) `;` a(d)),
        [Decomp(a, (nil `;` a(b)) // (a(c) `;` a(d))),
         Decomp(c, (a(a) `;` a(b)) // (nil `;` a(d)))] }
    ,   { sync(a(a) `;` a(b)) // (a(c) `;` a(d)),
        [Decomp(a, (nil `;` a(b)) `;` (nil // (a(c) `;` a(d)))),
         Decomp(c, sync(a(a) `;` a(b)) // (nil `;` a(d)))] }
    ,   { star(a(a) `;` a(b)),
        [Decomp(a, (nil `;` a(b)) `;` star(a(a) `;` a(b)))] }
    ] `with_type` list({prog(prim), _}),
    Test = (pred(P::in, Exp::in, Succ::out) is det :-
        T = next2(P),
        S = s0,
        Ds = tree.tree_to_list(tree.force(prgolog.pickbest(S), T)),
        sort(Ds, Ds1),
        sort(Exp, Exp1),
        ( if Ds1 = Exp1 then Succ = yes else throw({Ds, Exp}) )
    ),
    list.foldl((func({P, Exp}, M) = N :-
        Test(P, Exp, Succ),
        N = M + ( if Succ = yes then 1 else 0 )
    ), Inputs, 0) = _Successes,
    %format("%d successes\n", [i(Successes)], !IO),
    true.

%-----------------------------------------------------------------------------%

test_final(!IO) :-
    (   if final(nil `with_type` prog(prim))
        then true else throw("final seq")
    ),
    (   if not final(a(a) `with_type` prog(prim))
        then true else throw("final seq")
    ),
    (   if not final(a(impossible) `with_type` prog(prim))
        then true else throw("final seq")
    ),
    (   if not final(a(a) `;` a(b) `;` a(c) `;` a(d))
        then true else throw("final seq")
    ),
    (   if final(star(a(a) `;` a(b) `;` a(c) `;` a(d)))
        then true else throw("final star-seq")
    ),
    (   if not final(star(a(a) `;` a(b) `;` a(c)) `;` a(d))
        then true else throw("final star-seq")
    ),
    (   if final(star(a(a) or a(b)) or star(a(c)) or a(d))
        then true else throw("final star-nondet")
    ),
    (   if final(star(a(a) or a(b)) `;` star(a(c)) or a(d))
        then true else throw("final star-nondet-seq")
    ),
    true.

%-----------------------------------------------------------------------------%

:- func fixpoint(func(T) = T, T) = T.
:- mode fixpoint(in, in) = out is det.

fixpoint(F, X0) = ( if X = X0 then X else fixpoint(F, X) ) :- X = F(X0).


test_value(!IO) :-
    some [V, E] (
        V = value(a(a) `with_type` prog(prim), s0, 1),
        E = {0.0, 1},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(a) `;` a(a) `;` a(a)) or a(a)) `with_type` prog(prim), s0, 0),
        E = {0.0, 0},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(a) `;` a(a) `;` a(a)) or a(a)) `with_type` prog(prim), s0, 10),
        E = {0.0, 10},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(b) `;` a(b) `;` a(b)) or a(b)) `with_type` prog(prim), s0, 0),
        E = {0.0, 0},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(b) `;` a(b) `;` a(b)) or a(b)) `with_type` prog(prim), s0, 1),
        E = {1.0, 1},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(b) `;` a(b) `;` a(b)) or a(b)) `with_type` prog(prim), s0, 100),
        E = {3.0, 100},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(b) `;` a(b) `;` a(b)) or a(d)) `with_type` prog(prim), s0, 100),
        E = {3.0, 100},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(b) `;` a(b) `;` a(b)) or a(d)) `with_type` prog(prim), s0, 100),
        E = {3.0, 100},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(b) `;` a(b) `;` a(b)) or a(e)) `with_type` prog(prim), s0, 100),
        E = {4.0, 100},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value((a(e) or (a(b) `;` a(b) `;` a(b)) or a(d)) `with_type` prog(prim), s0, 100),
        E = {4.0, 100},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(d) // a(e)) or star(a(b)) or a(d) or a(c)) `with_type` prog(prim), s0, 5),
        E = {7.0, 5},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value((a(e) or star(a(b)) or a(d) or a(c)) `with_type` prog(prim), s0, 100),
        E = {100.0, 100},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(d) // a(e)) or (a(e) `;` a(d) `;` a(impossible))) `with_type` prog(prim), s0, 100),
        E = {7.0, 100},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(d) // a(e)) or (a(e) `;` a(e) `;` a(impossible))) `with_type` prog(prim), s0, 100),
        E = {8.0, 2},
        ( if V = E then true else throw({E, V}) )
    ),
    % For the following pickbest-tests:
    % The set_val/2 action stores a float value under a string key.
    % This value can be accessed with get_val/3 (the second parameter is the
    % default value).
    % Now we execute a set_val/2 action in the pick and the set value has effect
    % on the reward by inc_reward/1 + get_val/3 combination.
    % The reward is -1*X*X + 10 so that the peak is at X=0.0 and the reward is
    % at this point is 10.0.
    some [V, E, Next] (
        %Next = (func(X, Val, Cmp) = ( if Cmp(Val(X+1.0), Val(X)) = (>) then X+1.0 else X )),
        Next = (func(X0, Val, Cmp) = fixpoint((func(X) = ( if Cmp(Val(X+1.0), Val(X)) = (>) then X+1.0 else X )), X0)),
        V = value(with_type(
                    pickbest(Next, -3.0, func(X) = a(set_val("x", X))) `;`
                      a(a) `;`
                      b(func(S) = inc_reward(-1.0*(R*R)+10.0) is det :- R = get_val("x", -1.0, S))
                  , prog(prim)), s0, 100),
        E = {10.0, 100},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E, Next] (
        %Next = (func(X, Val, Cmp) = ( if Cmp(Val(X+1.0), Val(X)) = (>) then X+1.0 else X )),
        Next = (func(X0, Val, Cmp) = fixpoint((func(X) = ( if Cmp(Val(X+1.0), Val(X)) = (>) then X+1.0 else X )), X0)),
        V = value(with_type(
                    pickbest(Next, -3.0, func(X) = a(set_val("x", X))) `;`
                      a(a) `;`
                      b(func(S) = inc_reward(-1.0*(R*R)+10.0) is det :- R = get_val("x", -1.0, S))
                  , prog(prim)), s0, 3),
        E = {10.0, 3},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E, Next] (
        %Next = (func(X, Val, Cmp) = ( if Cmp(Val(X+1.0), Val(X)) = (>) then X+1.0 else X )),
        Next = (func(X0, Val, Cmp) = fixpoint((func(X) = ( if Cmp(Val(X+1.0), Val(X)) = (>) then X+1.0 else X )), X0)),
        V = value(with_type(
                    pickbest(Next, -3.0, func(X) = a(set_val("x", X))) `;`
                      a(a) `;`
                      b(func(S) = inc_reward(-1.0*(R*R)+10.0) is det :- R = get_val("x", -1.0, S))
                  , prog(prim)), s0, 2),
        E = {0.0, 2},
        ( if V = E then true else throw({E, V}) )
    ),
    true.

test_trans_atom(!IO) :-
    some [A] (
        A = prim(a) `with_type` atom(prim),
        ( if trans_atom(A, s0, do(a, s0)) then true else throw(A) )
    ),
    some [B] (
        B = primf((func(_) = a)) `with_type` atom(prim),
        ( if trans_atom(B, s0, do(a, s0)) then true else throw(B) )
    ),
    some [T] (
        T = test((func(_) = yes)) `with_type` atom(prim),
        ( if trans_atom(T, s0, s0) then true else throw(T) )
    ),
    true.

test_trans(!IO) :-
    some [P] (
        P = a(a) `with_type` prog(prim),
        ( if trans(P, s0, nil, do(a, s0)) then true else throw(P) )
    ),
    some [P] (
        P = a(a) `with_type` prog(prim),
        ( if trans(P, s0, nil, do(a, s0)) then true else throw(P) )
    ),
    some [P] (
        P = (a(impossible) or a(a) or a(b) or a(c) or a(d) or a(e)) `with_type` prog(prim),
        ( if trans(P, s0, nil, do(e, s0)) then true else throw(P) )
    ),
    some [P] (
        P = (a(e) or a(d) or a(c) or a(b) or a(a) or a(impossible)) `with_type` prog(prim),
        ( if trans(P, s0, nil, do(e, s0)) then true else throw(P) )
    ),
    some [P] (
        P = (a(e2) or a(e) or a(d) or a(c) or a(b) or a(a) or a(impossible)) `with_type` prog(prim),
        ( if trans(P, s0, nil, do(e, s0)) then true else throw(P) )
    ),
    some [P, Next] (
        %Next = (func(X, Val, Cmp) = ( if Cmp(Val(X+1.0), Val(X)) = (>) then X+1.0 else X )),
        Next = (func(X0, Val, Cmp) = fixpoint((func(X) = ( if Cmp(Val(X+1.0), Val(X)) = (>) then X+1.0 else X )), X0)),
        P = pickbest(Next, -3.0, func(X) = a(set_val("x", X))) `;`
            a(a) `;`
            b(func(S) = inc_reward(-1.0*(R*R)+10.0) is det :- R = get_val("x", 0.0, S))
            `with_type` prog(prim),
        ( if trans(P, s0, _, do(set_val("x", 0.0), s0)) then true else throw(P) )
    ),
    true.

test_final2(!IO) :-
    some [P] (
        P = nil `with_type` prog(prim),
        ( if final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = a(a) `;` nil `with_type` prog(prim),
        ( if not final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = nil `;` a(a) `with_type` prog(prim),
        ( if not final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = a(a) `with_type` prog(prim),
        ( if not final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = a(b) `with_type` prog(prim),
        ( if not final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = a(impossible) `with_type` prog(prim),
        ( if not final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = a(a) `;` a(b) `with_type` prog(prim),
        ( if not final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = a(a) `;` a(b) `;` a(impossible) `with_type` prog(prim),
        ( if not final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = (a(a) or a(b)) `with_type` prog(prim),
        ( if not final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = (a(impossible) or nil) `with_type` prog(prim),
        ( if final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = (nil or a(impossible)) `with_type` prog(prim),
        ( if final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = (a(a) // a(b)) `with_type` prog(prim),
        ( if not final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = star(a(a)) `with_type` prog(prim),
        ( if final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = star(a(a) `;` a(a) `;` a(a) `;` a(b)) `with_type` prog(prim),
        ( if final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = star(a(b)) `with_type` prog(prim),
        ( if not final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = star(a(impossible) `;` a(b)) `with_type` prog(prim),
        ( if final(P, s0) then true else throw(P) )
    ),
    some [P] (
        P = star(a(b) `;` a(impossible)) `with_type` prog(prim),
        % XXX actually this is not the intended outcome, or is it?
        ( if not final(P, s0) then true else throw(P) )
    ),
    true.

%-----------------------------------------------------------------------------%
:- end_module prgolog.test.
%-----------------------------------------------------------------------------%

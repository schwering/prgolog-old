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

:- type prim ---> a ; b ; c ; d ; e ; impossible.

:- instance bat(prim) where [
    poss(A, _) :- A \= impossible,
    reward(_, s0) = 0.0,
    reward(P, do(A, S)) = R + reward(P, S) :-
        (   A = a, R = 0.0
        ;   A = b, R = 1.0
        ;   A = c, R = 2.0
        ;   A = d, R = 3.0
        ;   A = e, R = 4.0
        ;   A = impossible, R = 0.0
        ),
    lookahead(_) = 3
].

%-----------------------------------------------------------------------------%

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
        Ds = next(P),
        sort(Ds, Ds1),
        sort(Exp, Exp1),
        ( if Ds1 = Exp1 then Succ = yes else throw({Ds, Exp}) )
    ),
    foldl((func({P, Exp}, M) = N :-
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
    ,   { atomic(a(a) `;` a(b)) // (a(c) `;` a(d)),
        [Decomp(a, (nil `;` a(b)) `;` (nil // (a(c) `;` a(d)))),
         Decomp(c, atomic(a(a) `;` a(b)) // (nil `;` a(d)))] }
    ,   { star(a(a) `;` a(b)),
        [Decomp(a, (nil `;` a(b)) `;` star(a(a) `;` a(b)))] }
    ] `with_type` list({prog(prim), _}),
    Test = (pred(P::in, Exp::in, Succ::out) is det :-
        Ds = next2(P),
        sort(Ds, Ds1),
        sort(Exp, Exp1),
        ( if Ds1 = Exp1 then Succ = yes else throw({Ds, Exp}) )
    ),
    foldl((func({P, Exp}, M) = N :-
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

test_value(!IO) :-
    some [V, E] (
        V = value(((a(a) `;` a(a) `;` a(a)) or a(a)) `with_type` prog(prim), s0, 0),
        E = {0.0, 0},
        ( if V = E then true else throw({E, V}) )
    ),
    some [V, E] (
        V = value(((a(a) `;` a(a) `;` a(a)) or a(a)) `with_type` prog(prim), s0, 10),
        E = {0.0, 0},
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
        % actually this is not the intended outcome
        ( if not final(P, s0) then true else throw(P) )
    ),
    true.

%-----------------------------------------------------------------------------%
:- end_module prgolog.test.
%-----------------------------------------------------------------------------%

% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0

:- module golog.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.
:- import_module solutions.
:- import_module string.

:- type fluent(A) == pred(gsit(A)).

%:- type gaction
%    --->    act1.

:- type gproc(A)
    --->    proc1.

:- type gcond(A)
    --->    and(gcond(A), gcond(A))
    ;       or(gcond(A), gcond(A))
    ;       if(gcond(A), gcond(A))
    ;       iff(gcond(A), gcond(A))
    ;       neg(gcond(A))
    ;       fluent(fluent(A)).

:- inst semidet_gcond
    --->    and(semidet_gcond, semidet_gcond)
    ;       or(semidet_gcond, semidet_gcond)
    ;       if(semidet_gcond, semidet_gcond)
    ;       iff(semidet_gcond, semidet_gcond)
    ;       neg(semidet_gcond)
    ;       fluent(pred(in) is semidet).

:- inst nondet_gcond
    --->    and(nondet_gcond, nondet_gcond)
    ;       or(nondet_gcond, nondet_gcond)
    ;       if(nondet_gcond, nondet_gcond)
    ;       iff(nondet_gcond, nondet_gcond)
    ;       neg(nondet_gcond)
    ;       fluent(pred(in) is nondet).
            % nondet macht keinen Sinn, da es nondet. bzgl. der Sit.var. ist;
            % gewuenscht waere nondet auch bzgl. der im Lambda-Ausdruck
            % versteckten Argumente

:- type gcode(A)
    --->    seq(gcode(A), gcode(A))
    ;       nondet(gcode(A), gcode(A))
    ;       conc(gcode(A), gcode(A))
    ;       test(gcond(A))
    ;       if(gcond(A), gcode(A), gcode(A))
    ;       while(gcond(A), gcode(A))
    ;       star(gcode(A))
    ;       prim(A)
    ;       proc(gproc(A))
    ;       nil.

:- inst semidet_gcode
    --->    seq(semidet_gcode, semidet_gcode)
    ;       test(semidet_gcond)
    ;       if(semidet_gcond, semidet_gcode, semidet_gcode)
    ;       while(semidet_gcond, semidet_gcode)
    ;       prim(ground)
    ;       proc(ground)
    ;       nil.

:- inst nondet_gcode
    --->    seq(nondet_gcode, nondet_gcode)
    ;       nondet(nondet_gcode, nondet_gcode)
    ;       conc(nondet_gcode, nondet_gcode)
    ;       test(nondet_gcond)
    ;       if(nondet_gcond, nondet_gcode, nondet_gcode)
    ;       while(nondet_gcond, nondet_gcode)
    ;       star(nondet_gcode)
    ;       prim(ground)
    ;       proc(ground)
    ;       nil.

:- type gsit(A)
    --->    s0
    ;       do(A, gsit(A)).


:- pred poss(A, gsit(A)).
:- mode poss(in, in) is det.

poss(_, _).



:- pred semidet_proc(gproc(A), gcode(A)).
:- mode semidet_proc(in, in(semidet_gcode)) is semidet.
:- mode semidet_proc(in, out(semidet_gcode)) is det.

semidet_proc(P, E) :-
    (   P = proc1,
        E = nil
    ).



:- pred nondet_proc(gproc(A), gcode(A)).
:- mode nondet_proc(in, in(nondet_gcode)) is semidet.
:- mode nondet_proc(in, out(nondet_gcode)) is det.

nondet_proc(P, E) :-
    (   P = proc1,
        E = nil
    ).



:- pred proc(gproc(A), gcode(A)).
:- mode proc(in, in(nondet_gcode)) is semidet.
:- mode proc(in, out(nondet_gcode)) is det.

proc(P, E) :-
    (   P = proc1,
        E = nil
    ).



:- pred to_nondet_cond(gcond(A), gcond(A)).
:- mode to_nondet_cond(in(semidet_gcond), out(nondet_gcond)) is det.
:- mode to_nondet_cond(in(nondet_gcond), out(nondet_gcond)) is det.
:- pragma promise_pure(to_nondet_cond/2).

to_nondet_cond(P::in(semidet_gcond), Q::out(nondet_gcond)) :-
    (   P = and(P1, P2),
        Q = and(Q1, Q2),
        to_nondet_cond(P1, Q1),
        to_nondet_cond(P2, Q2)
    ;   P = or(P1, P2),
        Q = or(Q1, Q2),
        to_nondet_cond(P1, Q1),
        to_nondet_cond(P2, Q2)
    ;   P = if(P1, P2),
        Q = if(Q1, Q2),
        to_nondet_cond(P1, Q1),
        to_nondet_cond(P2, Q2)
    ;   P = iff(P1, P2),
        Q = iff(Q1, Q2),
        to_nondet_cond(P1, Q1),
        to_nondet_cond(P2, Q2)
    ;   P = neg(P1),
        Q = neg(Q1),
        to_nondet_cond(P1, Q1)
    ;   P = fluent(F),
        Q = fluent((pred(S::in) is nondet :- F(S)))
    ).

to_nondet_cond(P::in(nondet_gcond), Q::out(nondet_gcond)) :-
    P = Q.



:- pred to_nondet_code(gcode(A), gcode(A)).
:- mode to_nondet_code(in(semidet_gcode), out(nondet_gcode)) is det.
:- mode to_nondet_code(in(nondet_gcode), out(nondet_gcode)) is det.
:- pragma promise_pure(to_nondet_code/2).

to_nondet_code(E::in(semidet_gcode), F::out(nondet_gcode)) :-
    (   E = seq(E1, E2),
        F = seq(F1, F2),
        to_nondet_code(E1, F1),
        to_nondet_code(E2, F2)
    ;   E = test(P),
        F = test(Q),
        to_nondet_cond(P, Q)
    ;   E = if(P, E1, E2),
        F = if(Q, F1, F2),
        to_nondet_cond(P, Q),
        to_nondet_code(E1, F1),
        to_nondet_code(E2, F2)
    ;   E = while(P, E1),
        F = while(Q, F1),
        to_nondet_cond(P, Q),
        to_nondet_code(E1, F1)
    ;   E = prim(A),
        F = prim(A)
    ;   E = proc(N),
        F = proc(N)
    ;   E = nil,
        F = nil
    ).

to_nondet_code(E::in(nondet_gcode), F::out(nondet_gcode)) :-
    E = F.



:- pred holds(gcond(A), gsit(A)).
:- mode holds(in(semidet_gcond), in) is semidet.
:- mode holds(in(nondet_gcond), in) is semidet.

holds(C, S) :-
    (   C = and(P, Q),
        holds(P, S),
        holds(Q, S)
    ;   C = or(P, Q),
        (   holds(P, S)
        ;   holds(Q, S)
        )
    ;   C = if(P, Q),
        holds(or(neg(P), Q), S)
    ;   C = iff(P, Q),
        holds(and(if(P, Q), if(Q, P)), S)
    ;   C = neg(neg(P)),
        holds(P, S)
    ;   C = neg(and(P, Q)),
        holds(or(neg(P), neg(Q)), S)
    ;   C = neg(or(P, Q)),
        holds(and(neg(P), neg(Q)), S)
    ;   C = neg(if(P, Q)),
        holds(neg(or(neg(P), Q)), S)
    ;   C = neg(iff(P, Q)),
        holds(neg(and(if(P, Q), if(Q, P))), S)
    ;   C = neg(P),
        P = fluent(_),
        not holds(P, S)
    ;   C = fluent(P),
        P(S)
    ).


%:- pred do(gcode(A), gsit(A), gsit(A)).
%%:- mode do(in(semidet_gcode), in, out) is semidet.
%:- mode do(in(nondet_gcode), in, out) is nondet.

%do(E, S, S1) :-
%    do(E, nil, S, S1).


%:- pred do(gcode(A), gcode(A), gsit(A), gsit(A)).
%%:- mode do(in(semidet_gcode), in(semidet_gcode), in, out) is semidet.
%:- mode do(in(nondet_gcode), in(nondet_gcode), in, out) is nondet.

%do(E, R, S, S1) :-
%    (   E = R
%    ;   E \= R,
%        trans(E, R0, S, S0),
%        trans(R0, R, S0, S1)
%    ).


%:- pred foo(gcode(A), gcode(A)).
%:- mode foo(in(semidet_gcode), out(semidet_gcode)) is det.
%:- mode foo(in(semidet_gcode), out(nondet_gcode)) is det.

%foo(E, F) :-
%    E = F.



:- pred trans(gcode(A), gcode(A), gsit(A), gsit(A)).
%:- mode trans(in(semidet_gcode), in(semidet_gcode), in, out) is semidet.
%:- mode trans(in(nondet_gcode), in(nondet_gcode), in, out) is nondet.
%:- mode trans(in(semidet_gcode), out(semidet_gcode), in, out) is nondet.
:- mode trans(in(nondet_gcode), out(nondet_gcode), in, out) is nondet.

trans(E, R, S, S1) :-
    (   (   E = nil `seq` E0
        ;   E = E0 `seq` nil
        ;   E = nil `nondet` E0
        ;   E = E0 `nondet` nil
        ;   E = nil `conc` E0
        ;   E = E0 `conc` nil
        ),
        trans(E0, R, S, S1)
    ;   E = nil,
        R = nil,
        S = S1
    ;   E = E1 `seq` E2,
        E1 \= nil,
        E2 \= nil,
        linearize(E1 `seq` E2, E3 `seq` E4),
        trans(E3, R1, S, S1),
        R = R1 `seq` E4
    ;   E = E1 `nondet` E2,
        E1 \= nil,
        E2 \= nil,
        (   trans(E1, R, S, S1)
        ;   trans(E2, R, S, S1)
        )
    ;   E = E1 `conc` E2,
        E1 \= nil,
        E2 \= nil,
        (   R = R1 `conc` E2,
            trans(E1, R1, S, S1)
        ;   R = E1 `conc` R1,
            trans(E1, R1, S, S1)
        )
    ;   E = test(P),
        R = nil,
        S = S1,
        holds(P, S)
    ;   E = if(P, E1, E2),
        (   if      holds(P, S)
            then    trans(E1, R, S, S1)
            else    trans(E2, R, S, S1)
        )
    ;   E = star(E1),
        (   R = nil,
            S = S1
        ;   R \= nil,
            trans(E1 `seq` star(E1), R, S, S1)
        )
    ;   E = while(P, E1),
        (   if
                holds(P, S)
            then
                trans(E1, R1, S, S1),
                R = R1 `seq` E
            else
                R = nil,
                S = S1
        )
    ;   E = proc(_),
        proc_body(E, E2),
        trans(E2, R, S, S1)
    ;   E = prim(A),
        R = nil,
        poss(A, S),
        S1 = do(A, S)
    ).



:- pred proc_body(gcode(A), gcode(A)).
%:- mode proc_body(in(semidet_gcode), out(semidet_gcode)) is semidet.
:- mode proc_body(in(nondet_gcode), out(nondet_gcode)) is semidet.
%:- pragma promise_pure(proc_body/2).

%proc_body(Call::in(semidet_gcode), E::out(semidet_gcode)) :-
    %Call = proc(N),
    %semidet_proc(N, E).

proc_body(Call::in(nondet_gcode), E::out(nondet_gcode)) :-
    Call = proc(N),
    (   if      semidet_proc(N, E0)
        then    to_nondet_code(E0, E)
        else    nondet_proc(N, E)
    ).



:- pred linearize(gcode(A), gcode(A)).
:- mode linearize(in(semidet_gcode), out(semidet_gcode)) is det.
:- mode linearize(in(nondet_gcode), out(nondet_gcode)) is det.

linearize(Old, New) :-
    (   if
            Old = (L1 `seq` L2) `seq` R1
        then
            linearize(L1, L),
            linearize(L2 `seq` R1, R),
            New = L `seq` R
        else if
            Old = L `seq` R1
        then
            linearize(R1, R),
            New = L `seq` R
        else
            Old = New
    ).



main(!IO) :-
    io.format("na, du?\n", [], !IO).


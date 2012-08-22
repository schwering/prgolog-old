%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: prgolog.nice.m.
% Main author: schwering.
%
% Wrappers for typical constructor combinations which provide a cleaner way to
% construct Golog programs.
%
% For example, to get a program that consists of a single primitive action, we
% need to build an atom from the action, a pseudo atom from the atom, and a
% program from the pseudo atom.
% The function a/1 below provides a shorthand for this; functions b/1, t/1, p/1
% behave appropriately for stochastic actions, test actions, and procedure
% calls, respectively.
%
% Additionally, functions to combine programs using the language features such
% as sequence and nondeterministic branch are provided.
% Note that no function star/1 is defined, because the constructor star/1 from
% the prgolog module suffices.
% The function pick/3 isn't implemented yet, because I'm still thinking about
% an elegant way to do this. This pick/3 would be a mere macro expansion
% anyway.
%
%-----------------------------------------------------------------------------%

:- module prgolog.nice.

:- interface.

:- import_module list.
:- use_module term.

%-----------------------------------------------------------------------------%

:- type conf(A, B, P) ---> conf(rest :: prog(A, B, P), sit :: sit(A)).

:- func init(prog(A, B, P)) = conf(A, B, P).
:- mode init(in) = out is det.

:- pred trans(conf(A, B, P), conf(A, B, P)) <= bat(A, B, P).
:- mode trans(in, out) is semidet.

:- pred final(conf(A, B, P)) <= bat(A, B, P).
:- mode final(in) is semidet.

:- pred do(conf(A, B, P), sit(A)) <= bat(A, B, P).
:- mode do(in, out) is semidet.

%-----------------------------------------------------------------------------%

:- func a(A) = prog(A, B, P) <= bat(A, B, P).
:- mode a(in) = out is det.

:- func b(B) = prog(A, B, P) <= bat(A, B, P).
:- mode b(in) = out is det.

:- func t(relfluent(A)) = prog(A, B, P) <= bat(A, B, P).
:- mode t(in) = out is det.

:- func p(P) = prog(A, B, P) <= bat(A, B, P).
:- mode p(in) = out is det.

:- func prog(A, B, P) `;` prog(A, B, P) = prog(A, B, P) <= bat(A, B, P).
:- mode in `;` in = out is det.

:- func prog(A, B, P) // prog(A, B, P) = prog(A, B, P) <= bat(A, B, P).
:- mode in // in = out is det.

:- func (prog(A, B, P) or prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode (in or in) = out is det.

:- func atomic(prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode atomic(in) = out is det.

:- func ifthen(relfluent(A), prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode ifthen(in, in) = out is det.

:- func ifthenelse(relfluent(A), prog(A, B, P), prog(A, B, P)) = prog(A, B, P)
   <= bat(A, B, P).
:- mode ifthenelse(in, in, in) = out is det.

:- func while(relfluent(A), prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode while(in, in) = out is det.

%-----------------------------------------------------------------------------%

:- typeclass pickable(V, T, A) where [
    func substitute(V, T, A) = A,
    mode substitute(in, in, in(I)) = out(I) is det
].

:- func pick(V, list(T), prog(A, B, P)) = prog(A, B, P)
    <= (bat(A, B, P),
        pickable(V, T, A), pickable(V, T, B),
        pickable(V, T, P), pickable(V, T, relfluent(A))).
:- mode pick(in, in(non_empty_list), in) = out is det.
:- mode pick(in, in, in) = out is semidet.

:- func pick2(term.var(T), list(T), prog(A, B, P)) = prog(A, B, P)
    <= bat(A, B, P).
:- mode pick2(in, in(non_empty_list), in) = out is det.
:- mode pick2(in, in, in) = out is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prgolog.fluent.

%-----------------------------------------------------------------------------%

init(P) = conf(P, s0).
trans(conf(P, S), conf(P1, S1)) :- trans(P, S, P1, S1).
final(conf(P, S)) :- final(P, S).
do(conf(P, S), S1) :- do(P, S, S1).

%-----------------------------------------------------------------------------%

a(A) = prgolog.pseudo_atom(prgolog.atom(prgolog.prim(A))).
b(B) = prgolog.pseudo_atom(prgolog.atom(prgolog.stoch(B))).
t(T) = prgolog.pseudo_atom(prgolog.atom(prgolog.test(T))).
p(P) = prgolog.proc(P).

%-----------------------------------------------------------------------------%

P1 `;` P2  = prgolog.seq(P1, P2).
P1 // P2 = prgolog.conc(P1, P2).
(P1 or P2)  = prgolog.non_det(P1, P2).
atomic(P) = prgolog.pseudo_atom(prgolog.complex(P)).
ifthen(T, P) = ifthenelse(T, P, nil).
ifthenelse(T, P1, P2) = ((t(T) `;` P1) or (t(neg(T)) `;` P2)).
while(T, P) = (t(T) `;` star(P) `;` t(neg(T))).

%-----------------------------------------------------------------------------%

:- func subst(term.var(T), T, prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode subst(in, in, in) = out is det.

subst(V, T, P) = P1 :-
    ProgTerm = term.type_to_term(P),
    Replacement = term.type_to_term(T),
    term.substitute(ProgTerm, V, Replacement, GroundProgTerm),
    term.det_term_to_type(GroundProgTerm, P1).


pick2(V, [T|Ts], P) = P0 :-
    P1 = subst(V, T, P),
    (   if      P2 = pick2(V, Ts, P)
        then    P0 = (P1 or P2)
        else    P0 = P1
    ).


pick(V, [T|Ts], P) = P1 :-
    if      P2 = pick(V, Ts, P)
    then    P1 = (replace(V, T, P) or P2)
    else    P1 = replace(V, T, P).


:- func replace(V, T, prog(A, B, P)) = prog(A, B, P)
    <= (bat(A, B, P),
        pickable(V, T, A), pickable(V, T, B),
        pickable(V, T, P), pickable(V, T, relfluent(A))).
:- mode replace(in, in, in) = out is det.

replace(V, T, seq(P0, P1)) = replace(V, T, P0) `seq` replace(V, T, P1).
replace(V, T, non_det(P0, P1)) = replace(V, T, P0) `non_det` replace(V, T, P1).
replace(V, T, conc(P0, P1)) = replace(V, T, P0) `conc` replace(V, T, P1).
replace(V, T, star(P)) = star(replace(V, T, P)).
replace(V, T, proc(P)) = proc(substitute(V, T, P)).
replace(V, T, pseudo_atom(atom(prim(A)))) =
    pseudo_atom(atom(prim(substitute(V, T, A)))).
replace(V, T, pseudo_atom(atom(stoch(B)))) =
    pseudo_atom(atom(stoch(substitute(V, T, B)))).
replace(V, T, pseudo_atom(atom(test(G)))) =
    pseudo_atom(atom(test(substitute(V, T, G)))).
replace(V, T, pseudo_atom(complex(P))) =
    pseudo_atom(complex(replace(V, T, P))).
replace(_, _, nil) = nil.

%-----------------------------------------------------------------------------%
:- end_module prgolog.nice.
%-----------------------------------------------------------------------------%

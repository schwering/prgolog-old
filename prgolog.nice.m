% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
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
% Christoph Schwering (schwering@gmail.com)

:- module prgolog.nice.

:- interface.

:- import_module list.

:- type conf(A, B, P) ---> conf(prog(A, B, P), sit(A)).
:- inst conf ---> conf(prog, ground).

%:- typeclass picker(T) where [
%    pred pick(gvar(D))
%].

:- typeclass pickable(A) where [
    func substitute(T, A) = A,
    mode substitute(in, in(I)) = out(I) is det
].

:- func init(prog(A, B, P)) = conf(A, B, P).
:- mode init(in(prog)) = out(conf) is det.

:- pred trans(conf(A, B, P), conf(A, B, P)) <= bat(A, B, P).
:- mode trans(in(conf), out(conf)) is semidet.

:- pred final(conf(A, B, P)) <= bat(A, B, P).
:- mode final(in(conf)) is semidet.

:- pred do(conf(A, B, P), sit(A)) <= bat(A, B, P).
:- mode do(in(conf), out) is semidet.

:- func a(A) = prog(A, B, P) <= bat(A, B, P).
:- mode a(in) = out(prog) is det.

:- func b(B) = prog(A, B, P) <= bat(A, B, P).
:- mode b(in) = out(prog) is det.

:- func t(relfluent(A)) = prog(A, B, P) <= bat(A, B, P).
:- mode t(in(relfluent)) = out(prog) is det.

:- func p(P) = prog(A, B, P) <= bat(A, B, P).
:- mode p(in) = out(prog) is det.

:- func prog(A, B, P) `;` prog(A, B, P) = prog(A, B, P) <= bat(A, B, P).
:- mode in(prog) `;` in(prog) = out(prog) is det.

:- func prog(A, B, P) // prog(A, B, P) = prog(A, B, P) <= bat(A, B, P).
:- mode in(prog) // in(prog) = out(prog) is det.

:- func (prog(A, B, P) or prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode (in(prog) or in(prog)) = out(prog) is det.

:- func atomic(prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode atomic(in(prog)) = out(prog) is det.

:- func ifthen(relfluent(A), prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode ifthen(in(relfluent), in(prog)) = out(prog) is det.

:- func ifthenelse(relfluent(A), prog(A, B, P), prog(A, B, P)) = prog(A, B, P)
   <= bat(A, B, P).
:- mode ifthenelse(in(relfluent), in(prog), in(prog)) = out(prog) is det.

:- func while(relfluent(A), prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode while(in(relfluent), in(prog)) = out(prog) is det.

:- func pick(list(T), prog(A, B, P)) = prog(A, B, P) <= (bat(A, B, P),
    pickable(A), pickable(B), pickable(P), pickable(relfluent(A))).
:- mode pick(in(non_empty_list), in(prog)) = out(prog) is det.
:- mode pick(in, in(prog)) = out(prog) is semidet.

%:- func star(prog(A, B, P)) = prog(A, B, P) is det <= bat(A, B, P).


:- implementation.

:- import_module prgolog.fluents.

init(P) = conf(P, s0).

trans(conf(P, S), conf(P1, S1)) :- trans(P, S, P1, S1).
final(conf(P, S)) :- final(P, S).
do(conf(P, S), S1) :- do(P, S, S1).

a(A) = prgolog.pseudo_atom(prgolog.atom(prgolog.prim(A))).
b(B) = prgolog.pseudo_atom(prgolog.atom(prgolog.stoch(B))).
t(T) = prgolog.pseudo_atom(prgolog.atom(prgolog.test(T))).
p(P) = prgolog.proc(P).

P1 `;` P2  = prgolog.seq(P1, P2).
P1 // P2 = prgolog.conc(P1, P2).
(P1 or P2)  = prgolog.non_det(P1, P2).
atomic(P) = prgolog.pseudo_atom(prgolog.complex(P)).
ifthen(T, P) = ifthenelse(T, P, nil).
ifthenelse(T, P1, P2) = ((t(T) `;` P1) or (t(neg(T)) `;` P2)).
while(T, P) = (t(T) `;` star(P) `;` t(neg(T))).

pick([X|Xs], P) = P1 :-
    if      P2 = pick(Xs, P)
    then    P1 = (replace(X, P) or P2)
    else    P1 = replace(X, P).

:- func replace(T, prog(A, B, P)) = prog(A, B, P) <= (bat(A, B, P),
    pickable(A), pickable(B), pickable(P), pickable(relfluent(A))).
:- mode replace(in, in(prog)) = out(prog) is det.

replace(V, seq(P0, P1)) = replace(V, P0) `seq` replace(V, P1).
replace(V, non_det(P0, P1)) = replace(V, P0) `non_det` replace(V, P1).
replace(V, conc(P0, P1)) = replace(V, P0) `conc` replace(V, P1).
replace(V, star(P)) = star(replace(V, P)).
replace(V, proc(P)) = proc(substitute(V, P)).
replace(V, pseudo_atom(atom(prim(A)))) = pseudo_atom(atom(prim(substitute(V, A)))).
replace(V, pseudo_atom(atom(stoch(B)))) = pseudo_atom(atom(stoch(substitute(V, B)))).
replace(V, pseudo_atom(atom(test(T)))) = pseudo_atom(atom(test(substitute(V, T)))).
replace(V, pseudo_atom(complex(P))) = pseudo_atom(complex(replace(V, P))).
replace(V, nil) = nil.

%star(P1) = prgolog.star(P1).


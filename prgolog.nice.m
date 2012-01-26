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

:- func pick(list(T), prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode pick(in(non_empty_list), in(prog)) = out(prog) is det.
:- mode pick(in, in(prog)) = out(prog) is semidet.

%:- func star(prog(A, B, P)) = prog(A, B, P) is det <= bat(A, B, P).


:- implementation.

:- import_module prgolog.fluents.

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

% Not implemented yet (in fact, replace/2 isn't implemented yet).
% This pick/3 would be a mere macro expansion anyway.
pick([X|Xs], P) = P1 :-
    if      P0 = pick(Xs, P)
    then    P1 = replace(X, P) `seq` P0
    else    P1 = replace(X, P).

:- func replace(T, prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode replace(in, in(prog)) = out(prog) is det.

replace(_, P) = P.

%star(P1) = prgolog.star(P1).


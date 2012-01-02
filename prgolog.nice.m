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
%
% Christoph Schwering (schwering@gmail.com)

:- module prgolog.nice.

:- interface.

:- func a(A) = prog(A, B, P) <= bat(A, B, P).
:- mode a(in) = out(prog) is det.

:- func b(B) = prog(A, B, P) <= bat(A, B, P).
:- mode b(in) = out(prog) is det.

:- func t(relfluent(A)) = prog(A, B, P) <= bat(A, B, P).
:- mode t(in(relfluent)) = out(prog) is det.

:- func p(P) = prog(A, B, P) <= bat(A, B, P).
:- mode p(in) = out(prog) is det.

:- func prog(A, B, P) `;` prog(A, B, P) = prog(A, B, P) is det <= bat(A, B, P).
:- mode in(prog) `;` in(prog) = out(prog) is det.

:- func prog(A, B, P) // prog(A, B, P) = prog(A, B, P) is det <= bat(A, B, P).
:- mode in(prog) // in(prog) = out(prog) is det.

:- func (prog(A, B, P) or prog(A, B, P)) = prog(A, B, P) is det <= bat(A, B, P).
:- mode (in(prog) or in(prog)) = out(prog) is det.

:- func atomic(prog(A, B, P)) = prog(A, B, P) <= bat(A, B, P).
:- mode atomic(in(prog)) = out(prog) is det.

%:- func star(prog(A, B, P)) = prog(A, B, P) is det <= bat(A, B, P).


:- implementation.

a(A) = prgolog.pseudo_atom(prgolog.atom(prgolog.prim(A))).
b(B) = prgolog.pseudo_atom(prgolog.atom(prgolog.stoch(B))).
t(T) = prgolog.pseudo_atom(prgolog.atom(prgolog.test(T))).
p(P) = prgolog.proc(P).

P1 `;` P2  = prgolog.seq(P1, P2).
P1 // P2 = prgolog.conc(P1, P2).
(P1 or P2)  = prgolog.non_det(P1, P2).
atomic(P) = prgolog.pseudo_atom(prgolog.complex(P)).
%star(P1) = prgolog.star(P1).


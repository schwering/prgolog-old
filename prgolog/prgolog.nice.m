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

:- type conf(A) ---> conf(rest :: prog(A), sit :: sit(A)).

:- func init(prog(A)) = conf(A).
:- mode init(in) = out is det.

:- pred trans(conf(A), conf(A)) <= bat(A).
:- mode trans(in, out) is semidet.

:- func trans(conf(A)) = conf(A) <= bat(A).
:- mode trans(in) = out is semidet.

:- pred final(conf(A)) <= bat(A).
:- mode final(in) is semidet.

:- pred do(conf(A), sit(A)) <= bat(A).
:- mode do(in, out) is semidet.

:- func do(conf(A)) = sit(A) <= bat(A).
:- mode do(in) = out is semidet.

%-----------------------------------------------------------------------------%

:- func a(A) = prog(A) <= bat(A).
:- mode a(in) = out is det.

:- func b(primf(A)) = prog(A) <= bat(A).
:- mode b(in) = out is det.

:- func t(relfluent(A)) = prog(A) <= bat(A).
:- mode t(in) = out is det.

:- func p(proc(A)) = prog(A) <= bat(A).
:- mode p(in) = out is det.

:- func prog(A) `;` prog(A) = prog(A) <= bat(A).
:- mode in `;` in = out is det.

:- func prog(A) // prog(A) = prog(A) <= bat(A).
:- mode in // in = out is det.

:- func (prog(A) or prog(A)) = prog(A) <= bat(A).
:- mode (in or in) = out is det.

:- func atomic(prog(A)) = prog(A) <= bat(A).
:- mode atomic(in) = out is det.

:- func ifthen(relfluent(A), prog(A)) = prog(A) <= bat(A).
:- mode ifthen(in, in) = out is det.

:- func ifthenelse(relfluent(A), prog(A), prog(A)) = prog(A) <= bat(A).
:- mode ifthenelse(in, in, in) = out is det.

:- func while(relfluent(A), prog(A)) = prog(A) <= bat(A).
:- mode while(in, in) = out is det.

:- func pickbest(maxi_func(T), T, pickprog(A, T)) = prog(A).
:- mode pickbest(in, in, in) = out is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prgolog.fluent.

%-----------------------------------------------------------------------------%

init(P) = conf(P, s0).
trans(conf(P, S), conf(P1, S1)) :- trans(P, S, P1, S1).
trans(Conf) = Conf1 :- trans(Conf, Conf1).
final(conf(P, S)) :- final(P, S).
do(conf(P, S), S1) :- do(P, S, S1).
do(Conf) = S1 :- do(Conf, S1).

%-----------------------------------------------------------------------------%

a(A) = pseudo_atom(atom(prim(A))).
b(B) = pseudo_atom(atom(primf(B))).
t(T) = pseudo_atom(atom(test(T))).
p(P) = proc(P).

%-----------------------------------------------------------------------------%

P1 `;` P2  = seq(P1, P2).
P1 // P2 = conc(P1, P2).
(P1 or P2)  = non_det(P1, P2).
atomic(P) = pseudo_atom(prgolog.complex(P)).
ifthen(T, P) = ifthenelse(T, P, nil).
ifthenelse(T, P1, P2) = ((t(T) `;` P1) or (t(neg(T)) `;` P2)).
while(T, P) = (t(T) `;` star(P) `;` t(neg(T))).
pickbest(F, I, P) = 'new pick'(F, I, P).

%-----------------------------------------------------------------------------%
:- end_module prgolog.nice.
%-----------------------------------------------------------------------------%

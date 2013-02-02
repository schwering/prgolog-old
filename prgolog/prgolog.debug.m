%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: prgolog.debug.m.
% Main author: schwering.
%
% Debugging predicates to get some insight how the interpreter makes its
% decisions.
%
%-----------------------------------------------------------------------------%

:- module prgolog.debug.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred print_decomps(sit(A)::in, prog(A)::in, io::di, io::uo) is det <= bat(A).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

print_decomps(S, P, !IO) :-
    format("best value %s\n", [s(string(value(P, S) : value))], !IO),
    pd(S, nil, P, 1, 1, _, !IO).


:- pred pd(sit(A)::in, prog(A)::in, prog(A)::in, int::in,
           int::in, int::out, io::di, io::uo) is det <= bat(A).

pd(S, P1, P2, Depth, !Counter, !IO) :-
    if Depth =< lookahead(S) then
        W = duplicate_char(' ', 4*Depth),
        Ds = tree.tree_to_list(tree.force(pickbest(S), next2(P2))),
        foldl2((pred(decomp(C, R)::in, !.I::in, !:I::out, !.SubIO::di, !:SubIO::uo) is det :-
            P = seq(P1, pseudo_atom(atom(C))),
            V = value(seq(P, R), S) : value,
            format("%s%4d %s  %s\n", [s(W), i(!.I), s(string(C)), s(string(V))], !SubIO),
            pd(S, P, R, Depth + 1, !.I + 1, !:I, !SubIO)
        ), Ds, !Counter, !IO)
    else
        true.

%-----------------------------------------------------------------------------%
:- end_module prgolog.debug.
%-----------------------------------------------------------------------------%

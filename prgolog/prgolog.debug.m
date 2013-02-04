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
:- import_module list.

%-----------------------------------------------------------------------------%

:- type format_pred == (pred(string, list(io.poly_type), io, io)).
:- inst format_pred == (pred(in, in, di, uo) is det).

:- pred print_prog(format_pred::in(format_pred), prog(A)::in,
                   io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- pred print_decomps(sit(A)::in, prog(A)::in, io::di, io::uo) is det <= bat(A).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%

:- func w(int) = string.

w(Depth) = duplicate_char(' ', 4*Depth).

%-----------------------------------------------------------------------------%

:- type last_op ---> seq ; non_det ; conc ; unary.

print_prog(Format, P, !IO) :-
    print(Format, P, seq, 0, !IO).


:- pred is_nil(prog(A)::in) is semidet.

is_nil(seq(P1, P2)) :- is_nil(P1), is_nil(P2).
is_nil(conc(P1, P2)) :- is_nil(P1), is_nil(P2).
is_nil(pseudo_atom(complex(P))) :- is_nil(P).
is_nil(nil).


:- pred print(format_pred::in(format_pred), prog(A)::in, last_op::in, int::in,
              io::di, io::uo) is det.

print(Format, seq(P1, P2), Last, Depth, !IO) :-
    Last1 = seq,
    Depth1 = ( if Last = Last1 then Depth else Depth + 1 ),
    ( if is_nil(P1), is_nil(P2) then
        print(Format, nil, Last1, Depth1, !IO)
      else if is_nil(P1) then
        print(Format, P2, Last, Depth, !IO)
      else if is_nil(P2) then
        print(Format, P1, Last, Depth, !IO)
      else
        ( if Last \= Last1 then Format("%sseq(\n", [s(w(Depth))], !IO) else true ),
        print(Format, P1, Last1, Depth1, !IO),
        print(Format, P2, Last1, Depth1, !IO),
        ( if Last \= Last1 then Format("%s)\n", [s(w(Depth))], !IO) else true )
    ).
print(Format, non_det(P1, P2), Last, Depth, !IO) :-
    Last1 = non_det,
    Depth1 = ( if Last = Last1 then Depth else Depth + 1 ),
    ( if Last \= Last1 then Format("%snon_det(\n", [s(w(Depth))], !IO) else true ),
    print(Format, P1, Last1, Depth1, !IO),
    print(Format, P2, Last1, Depth1, !IO),
    ( if Last \= Last1 then Format("%s)\n", [s(w(Depth))], !IO) else true ).
print(Format, conc(P1, P2), Last, Depth, !IO) :-
    Last1 = conc,
    Depth1 = ( if Last = Last1 then Depth else Depth + 1 ),
    ( if is_nil(P1), is_nil(P2) then
        print(Format, nil, Last1, Depth1, !IO)
      else if is_nil(P1) then
        print(Format, P2, Last, Depth, !IO)
      else if is_nil(P2) then
        print(Format, P1, Last, Depth, !IO)
      else
        ( if Last \= Last1 then Format("%sconc(\n", [s(w(Depth))], !IO) else true ),
        print(Format, P1, Last1, Depth1, !IO),
        print(Format, P2, Last1, Depth1, !IO),
        ( if Last \= Last1 then Format("%s)\n", [s(w(Depth))], !IO) else true )
    ).
print(Format, star(P), _, Depth, !IO) :-
    (   if      is_nil(P)
        then    true
        else    Format("%sstar(\n", [s(w(Depth))], !IO),
                print(Format, P, unary, Depth + 1, !IO),
                Format("%s)\n", [s(w(Depth))], !IO)
    ).
print(Format, pick(Maxi, X0, G), _, Depth, !IO) :-
    Format("%spick(%s, %s,\n", [s(w(Depth)), s(string(Maxi)), s(string(X0))], !IO),
    print(Format, G(X0), unary, Depth + 1, !IO),
    Format("%s)\n", [s(w(Depth))], !IO).
print(Format, proc(P), _, Depth, !IO) :-
    Format("%sproc(%s,\n", [s(w(Depth)), s(string(P))], !IO),
    print(Format, apply(P), unary, Depth + 1, !IO),
    Format("%s)\n", [s(w(Depth))], !IO).
print(Format, pseudo_atom(complex(P)), _, Depth, !IO) :-
    Format("%scomplex(\n", [s(w(Depth))], !IO),
    print(Format, P, unary, Depth + 1, !IO),
    Format("%s)\n", [s(w(Depth))], !IO).
print(Format, pseudo_atom(atom(A)), _, Depth, !IO) :-
    Format("%s%s\n", [s(w(Depth)), s(string(A))], !IO).
print(Format, nil, _, Depth, !IO) :-
    Format("%snil\n", [s(w(Depth))], !IO).

%-----------------------------------------------------------------------------%

print_decomps(S, P, !IO) :-
    format("best value %s\n", [s(string(value(P, S) : value))], !IO),
    pd(S, nil, P, 1, 1, _, !IO).


:- pred pd(sit(A)::in, prog(A)::in, prog(A)::in, int::in,
           int::in, int::out, io::di, io::uo) is det <= bat(A).

pd(S, P1, P2, Depth, !Counter, !IO) :-
    if Depth =< lookahead(S) then
        Ds = tree.tree_to_list(tree.force(pickbest(S), next2(P2))),
        foldl2((pred(decomp(C, R)::in, !.I::in, !:I::out, !.SubIO::di, !:SubIO::uo) is det :-
            P = seq(P1, pseudo_atom(atom(C))),
            V = value(seq(P, R), S) : value,
            format("%s%4d %s  %s\n", [s(w(Depth)), i(!.I), s(string(C)), s(string(V))], !SubIO),
            pd(S, P, R, Depth + 1, !.I + 1, !:I, !SubIO)
        ), Ds, !Counter, !IO)
    else
        true.

%-----------------------------------------------------------------------------%
:- end_module prgolog.debug.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: util.pipe.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module util.pipe.
:- interface.

:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type pipe(T).

:- pred init(pipe(T)::out, io::di, io::uo) is det.

:- pred put(pipe(T)::in, T::in, io::di, io::uo) is det.

:- pred try_take(pipe(T)::in, maybe(T)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%:- import_module string.
%:- import_module list.
%:- import_module times.
:- import_module thread.
:- import_module thread.mvar.
:- import_module thread.semaphore.

%-----------------------------------------------------------------------------%

:- type stream(T) ---> chain(T, stream(T)) ; singleton(T).
:- type pipe(T) == {mvar(stream(T)), semaphore}.


init({Var, Sem}, !IO) :-
    %thread_id(TID, !IO),
    %format("%d: pipe:init\n", [i(TID)], !IO),
    init(Var, !IO),
    init(Sem, !IO),
    signal(Sem, !IO).
    %format("%d: pipe:init done\n", [i(TID)], !IO).


:- func append(T, stream(T)) = stream(T).

append(Val, chain(Val0, Chain)) = chain(Val0, append(Val, Chain)).
append(Val, singleton(Val0)) = chain(Val0, singleton(Val)).


put({Var, Sem}, Val, !IO) :-
    %thread_id(TID, !IO),
    %format("%d: pipe:put\n", [i(TID)], !IO),
    wait(Sem, !IO),
    try_take(Var, MaybeRest, !IO),
    (   if      MaybeRest = yes(Rest)
        then    Stream = append(Val, Rest)
        else    Stream = singleton(Val)
    ),
    put(Var, Stream, !IO),
    signal(Sem, !IO).
    %format("%d: pipe:put done\n", [i(TID)], !IO).


try_take({Var, Sem}, MaybeVal, !IO) :-
    %thread_id(TID, !IO),
    %format("%d: pipe:try_take\n", [i(TID)], !IO),
    wait(Sem, !IO),
    try_take(Var, MaybeStream, !IO),
    (   MaybeStream = yes(chain(Val, Rest)),
        put(Var, Rest, !IO),
        MaybeVal = yes(Val)
    ;   MaybeStream = yes(singleton(Val)),
        MaybeVal = yes(Val)
    ;   MaybeStream = no,
        MaybeVal = no
    ),
    signal(Sem, !IO).
    %format("%d: pipe:try_take done\n", [i(TID)], !IO).

%-----------------------------------------------------------------------------%
:- end_module util.pipe.
%-----------------------------------------------------------------------------%

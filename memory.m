%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module memory.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module times.
:- import_module thread.
:- import_module thread.channel.
%:- import_module thread.mvar.
:- import_module thread.semaphore.

main(!IO) :-
    init(V, !IO),
    init(S, !IO),
    P0 = (pred(IO0::di, IO1::uo) is cc_multi :-
        some [!IO] (
            IO0 = !:IO,
            signal(S, !IO),
            put(V, 1000, !IO),
            repeat(10000, !IO),
            IO1 = !.IO
        )
    ),
    P1 = (pred(IO0::di, IO1::uo) is cc_multi :-
        some [!IO] (
            IO0 = !:IO,
            wait(S, !IO),
            take(V, _, !IO),
            repeat(10000, !IO),
            IO1 = !.IO
        )
    ),
    spawn(P0, !IO),
    spawn(P1, !IO).


:- pred repeat(int::in, io::di, io::uo) is det.

repeat(N, !IO) :-
    list2(10000, L, !IO),
    fgn(N, L, !IO),
    ( if N > 0 then repeat(N-1, !IO) else true ).


:- pred list1(int, list(int), io, io).
:- mode list1(in, out, di, uo) is det.

list1(N, Ns, !IO) :- (if N > 0 then list1(N-1, Ns0, !IO), Ns = [N | Ns0] else Ns = [] ).


:- pred list2(int, list(int), io, io).
:- mode list2(in, uo, di, uo) is det.

list2(N, Ns, !IO) :- (if N > 0 then list2(N-1, Ns0, !IO), copy(N, N0), Ns = [N0 | Ns0] else Ns = [] ).


:- pred fgn(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    fgn(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"
    IO1 = IO0;
").


:- pred fgn(int::in, list(int)::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    fgn(N::in, Vs::in, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness, may_not_duplicate, no_sharing],
"
    IO1 = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module memory.
%-----------------------------------------------------------------------------%

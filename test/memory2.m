%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module memory2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module thread.

main(!IO) :-
    ( if {1.0, 2} @< {2.0, 2} then io.format("Gut\n", [], !IO) else io.format("Schlecht\n", [], !IO) ),
    ( if {2.0, 2} = {2.0, 2} then io.format("Gut\n", [], !IO) else io.format("Schlecht\n", [], !IO) ),
    ( if {2.0, 2} @=< {2.0, 2} then io.format("Gut\n", [], !IO) else io.format("Schlecht\n", [], !IO) ),
    ( if {2.0, 2} @>= {2.0, 2} then io.format("Gut\n", [], !IO) else io.format("Schlecht\n", [], !IO) ),
    ( if {2.0, 2} @> {2.0, 2} then io.format("Schlecht\n", [], !IO) else io.format("Gut\n", [], !IO) ),
    ( if ordering({1.0, 2}, {2.0, 2}) = (<) then io.format("Gut\n", [], !IO) else io.format("Schlecht\n", [], !IO) ),
    ( if ordering({2.0, 2}, {2.0, 2}) = (=) then io.format("Gut\n", [], !IO) else io.format("Schlecht\n", [], !IO) ),
    ( if ordering({2.0, 2}, {2.0, 2}) = (>) then io.format("Schlecht\n", [], !IO) else io.format("Gut\n", [], !IO) ),
    ( if {-1.0, -2} @> {-2.0, -2} then io.format("Gut\n", [], !IO) else io.format("Schlecht\n", [], !IO) ),
    ( if {-2.0, -2} = {-2.0, -2} then io.format("Gut\n", [], !IO) else io.format("Schlecht\n", [], !IO) ),
    ( if {-2.0, -2} @=< {-2.0, -2} then io.format("Gut\n", [], !IO) else io.format("Schlecht\n", [], !IO) ),
    ( if {-2.0, -2} @>= {-2.0, -2} then io.format("Gut\n", [], !IO) else io.format("Schlecht\n", [], !IO) ),
    ( if {-2.0, -2} @< {-2.0, -2} then io.format("Schlecht\n", [], !IO) else io.format("Gut\n", [], !IO) ),
    P = (pred(Cont::out, IO0::di, IO1::uo) is det :-
        some [!TIO] (
            IO0 = !:TIO,
            foreign(Cont, !TIO),
            IO1 = !.TIO
        )
    ),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO),
    spawn(loop(P), !IO).


:- pred loop(pred(bool, io, io)::in(pred(out, di, uo) is det),
             io::di, io::uo) is cc_multi.

loop(P, !IO) :- P(Cont, !IO), ( if Cont = yes then loop(P, !IO) else true ).


:- pred foreign(bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    foreign(Cont::out, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
    %[will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
    %    does_not_affect_liveness, may_not_duplicate, no_sharing],
"
    static int ctr = 0;
    const int s[] = { 3, 4096, 1, 1, 1, 1, 1, 1029, 12409, 329, 9, 632, 64 };
    const int n = sizeof(s) / sizeof(s[0]);
    void **mem;
    int i, j;

    mem = malloc(n * sizeof(void *));
    for (i = 0; i < n; ++i) {
        mem[i] = malloc(s[i]);
        for (j = 0; j < i; ++j) {
            memset(mem[i], 0, s[i]);
            memcpy(mem[i], mem[j], s[i] < s[j] ? s[i] : s[j]);
        }
    }

    for (i = 0; i < n; ++i) {
        free(mem[i]);
    }
    free(mem);

    Cont = ++ctr < 1024*1024;

    IO1 = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module memory2.
%-----------------------------------------------------------------------------%

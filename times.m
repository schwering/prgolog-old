%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: times.m.
% Main author: schwering.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module times.

:- interface.

:- import_module io.

:- type tms.

:- pred times(tms::out, io::di, io::uo) is det.

:- func usertime(tms::in, tms::in) = (float::out) is det.
:- pred usertime(tms::in, tms::in, float::out) is det.

:- func systime(tms::in, tms::in) = (float::out) is det.
:- pred systime(tms::in, tms::in, float::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "#include <sys/times.h>").
:- pragma foreign_decl("C", "#include <stdio.h>").


:- pragma foreign_type("C", tms, "struct tms").

:- pragma foreign_proc("C",
    times(Tms::out, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure],
"
    times(&Tms);
    IO1 = IO0;
").


usertime(Tms1, Tms2) = Diff :- usertime(Tms1, Tms2, Diff).


:- pragma foreign_proc("C",
    usertime(Tms1::in, Tms2::in, Diff::out),
    [will_not_call_mercury, promise_pure],
"
    const clock_t c1 = Tms1.tms_utime + Tms1.tms_cutime;
    const clock_t c2 = Tms2.tms_utime + Tms2.tms_cutime;
    if (c1 > c2) {
        Diff = ((MR_Float) (c1 - c2)) / sysconf(_SC_CLK_TCK);
    } else {
        Diff = ((MR_Float) (c2 - c1)) / sysconf(_SC_CLK_TCK);
    }
").


systime(Tms1, Tms2) = Diff :- systime(Tms1, Tms2, Diff).


:- pragma foreign_proc("C",
    systime(Tms1::in, Tms2::in, Diff::out),
    [will_not_call_mercury, promise_pure],
"
    const clock_t c1 = Tms1.tms_stime + Tms1.tms_cstime;
    const clock_t c2 = Tms2.tms_stime + Tms2.tms_cstime;
    if (c1 > c2) {
        Diff = ((MR_Float) (c1 - c2)) / sysconf(_SC_CLK_TCK);
    } else {
        Diff = ((MR_Float) (c2 - c1)) / sysconf(_SC_CLK_TCK);
    }
").

%-----------------------------------------------------------------------------%
:- end_module times.
%-----------------------------------------------------------------------------%

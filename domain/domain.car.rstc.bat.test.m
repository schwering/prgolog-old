%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.bat.test.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.bat.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test_ntg(io::di, io::uo) is det.
:- pred test_ttc(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module arithmetic.
:- import_module arithmetic.impl.
:- import_module exception.
:- import_module maybe.
:- import_module solutions.
:- import_module string.
:- import_module domain.car.obs.
:- import_module domain.car.obs.stdin.

%-----------------------------------------------------------------------------%

:- pred init_stream(string::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    init_stream(Str::in, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    FILE *fp = fopen(Str, ""r"");
    domain__car__obs__stdin__set_stream(fp);
    IO1 = IO0;
").


:- pred close_stream(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close_stream(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    fclose(domain__car__obs__stdin__get_stream());
    domain__car__obs__stdin__set_stream(stdin);
    IO1 = IO0;
").

:- pred check(
    func(agent, agent, rstc.sit(float)) = float,
    pred(category),
    string, agent, agent, rstc.sit(float), io, io).
:- mode check(
    func(in, in, in) = out is semidet,
    pred(out) is multi,
    in, in, in, in, di, uo) is det.
:- mode check(
    func(in, in, in) = out is semidet,
    pred(out) is nondet,
    in, in, in, in, di, uo) is det.

check(F, P, Name, B, C, S, !IO) :-
    ( if Tmp1 = F(B, C, S) then Maybe = yes(Tmp1) else Maybe = no ),
    solutions((pred(Cat::out) is nondet :-
        Maybe = yes(Tmp2),
        P(Cat),
        Tmp2 `in` Cat
    ), CatList),
    format("%6.3f: %s(%s, %s) = ",
        [f(start(S)), s(Name), s(agent_to_string(B)), s(agent_to_string(C))], !IO),
    ( if Maybe = yes(Tmp3) then format("%6.3f", [f(Tmp3)], !IO) else write_string("undef", !IO) ),
    write_string(" in ", !IO),
    write(CatList, !IO),
    nl(!IO),
    true.


:- type kind ---> ntg ; ttc.

:- pred test_loop(kind::in, stream_state::di, stream_state::uo, io::di, io::uo) is det.

test_loop(Kind, !SS, !IO) :-
    some [Sit, Prog] (
        Sit = s0 `with_type` rstc.sit(float),
        Prog = nil `with_type` rstc.prog(float),
        next_obs(Msg, Sit, Prog, !SS, !IO)
    ),
    (   (   Msg = init_msg(Env)
        ;   some [T, D] ( Msg = obs_msg(obs(T, D)), Env = env(T, D) )
        ),
        S = do(init_env(Env), s0) `with_type` rstc.sit(float),
        (   Kind = ntg,
            check(ntg, (pred(Cat::out) is nondet :- ntg_cat(Cat)), "NTG", c, b, S, !IO)
        ;   Kind = ttc,
            check(ttc, (pred(Cat::out) is nondet :- ttc_cat(Cat)), "TTC", c, b, S, !IO)
        ),
        test_loop(Kind, !SS, !IO)
    ;   Msg = end_of_obs
    ).


test_ntg(!IO) :-
    some [StreamState] (
        init_stream("cars-main/torcs-overtake-7.log", !IO),
        init_obs_stream(stdin.source, 0, StreamState, !IO),
        test_loop(ntg, StreamState, _, !IO)
    ).

test_ttc(!IO) :-
    some [StreamState] (
        init_obs_stream(stdin.source, 0, StreamState, !IO),
        test_loop(ttc, StreamState, _, !IO),
        close_stream(!IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.bat.test.
%-----------------------------------------------------------------------------%

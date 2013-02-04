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
:- pred test_match_dist(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module domain.car.obs.
:- import_module domain.car.obs.stdin.
:- import_module exception.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module solutions.
:- import_module string.
:- import_module util.arithmetic.impl.

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
    func(agent, agent, rstc.sit(float)) = num(N),
    pred(category),
    string, agent, agent, rstc.sit(float), io, io)
    <= arithmetic.arithmetic(N).
:- mode check(
    func(in, in, in) = out is semidet,
    pred(out) is multi,
    in, in, in, in, di, uo) is det.
:- mode check(
    func(in, in, in) = out is semidet,
    pred(out) is nondet,
    in, in, in, in, di, uo) is det.

check(F, P, Name, B, C, S, !IO) :-
    some [Tmp] ( if Tmp = start(S) then Start = string(Tmp) else Start = "undef" ),
    some [Tmp] ( if Tmp = F(B, C, S) then Maybe = yes(Tmp) else Maybe = no ),
    solutions((pred(Cat::out) is nondet :-
        some [Tmp] (
            Maybe = yes(Tmp),
            P(Cat),
            Tmp `in` Cat
        )
    ), CatList),
% XXX What about this output? Are there some assertions about that?
%    format("%s: %s(%s, %s) = ",
%        [s(Start), s(Name), s(agent_to_string(B)), s(agent_to_string(C))], !IO),
%    ( if Maybe = yes(Tmp3) then format("%s", [s(string(Tmp3))], !IO) else write_string("undef", !IO) ),
%    write_string(" in ", !IO),
%    write(CatList, !IO),
%    nl(!IO),
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
        S = do(init_env(abs_env(Env)), s0) `with_type` rstc.sit(float),
        (   Kind = ntg,
            check(ntg, (pred(Cat::out) is nondet :- ntg_cat(Cat)), "NTG", h, d, S, !IO)
        ;   Kind = ttc,
            check(ttc, (pred(Cat::out) is nondet :- ttc_cat(Cat)), "TTC", h, d, S, !IO)
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

test_match_dist(!IO) :-
    F = (func(N) = arithmetic.to_float(det_basic(N))),
    Info1 = [pair(car.h, info(20.729, -2.209e-06, p(0.0, -1.678))),
             pair(car.d, info(20.741, -1.393e-05, p(12.807, -2.999)))],
    Info2 = [pair(car.h, info(19.371, -2.209e-06, p(113.999, -1.678))),
             pair(car.d, info(20.779, -1.182e-05, p(127.967, -2.999)))],
    Init = init_env(abs_env(env(5.044, Info1))),
    Accel = accel(car.h, num(1.0006164907030441)),
    Obs = match(obs(10.592, Info2)),
    Sits = [ prgolog.do(Obs, prgolog.do(Accel, prgolog.do(Init, s0)))
           , prgolog.do(Obs, prgolog.do(Accel, prgolog.do(Init, s0)))
           , prgolog.do(Obs, prgolog.do(Accel, prgolog.do(Init, s0)))
           ],
    Infos = [ Info1, Info2 ],
    foldl((pred(S::in, !.IO0::di, !:IO0::uo) is det :-
        foldl((pred(Info::in, !.IO1::di, !:IO1::uo) is det :-
            D = F(bat.match_dist(Info, S)),
            ( if D < 0.0 ; D > 1.0 then throw({"distance not in [0, 1]", D, Info, S}) else true ),
            some [A] ( if S = do(A, _), A = init_env(abs_env(env(_, Info))), D \= 0.0 then throw({"distance not in 0", D, Info, S}) else true ),
            some [A] ( if S = do(A, _), A \= init_env(abs_env(env(_, Info))), D = 0.0 then throw({"distance is 0", D, Info, S}) else true )
        ), Infos, !IO0)
    ), Sits, !IO),
    true.

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.bat.test.
%-----------------------------------------------------------------------------%

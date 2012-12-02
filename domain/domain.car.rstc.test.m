%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.test.m.
% Main author: schwering.
%
% I've thoroughly looked at the RSTC prototype implemented in ECLiPSe-CLP with
% COIN-OR constraint solver. For that reason this unit test basically checks
% whether it gets the same result als ECLiPSe-CLP.
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test_1(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module arithmetic.
:- import_module arithmetic.impl.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

:- func x(agent) = float.

x(b) = 1.0.
x(c) = 5.0.
x(d) = 3.0.


:- func v(agent) = float.

v(b) = 3.0.
v(c) = 2.0.
v(d) = 1.0.


:- func init_sit = rstc.sit(float).

init_sit = do(init_env(env(0.0, L)), s0) :-
    L = [(b - info(v(b), 0.0, p(x(b), 1.0)))
        ,(c - info(v(c), 0.0, p(x(c), 1.0)))
        ,(d - info(v(d), 0.0, p(x(d), 1.0)))].


:- type test(N) ---> test(func(rstc.sit(N)) = N, N, string).
:- inst test ---> test(func(in) = out is semidet, ground, ground).


:- pred check_sit(rstc.sit(float), list(test(float)), io, io).
:- mode check_sit(in, list_skel_in(test), di, uo) is det.

check_sit(_, [], !IO).
check_sit(S, [test(Func, Exp, Str) | Rest], !IO) :-
    (   if      float.abs(Func(S) `float.'-'` Exp) `float.'<'` 0.01
        then    true
        else    write(S, !IO), nl(!IO),
                (   if      some [Val] Val = Func(S)
                    then    format("%s = %f != %f\n", [s(Str), f(Val), f(Exp)], !IO)
                    else    format("%s = undef != %f\n", [s(Str), f(Exp)], !IO)
                )
    ),
    check_sit(S, Rest, !IO).


    % (Beginning of ECLiPSE-CLP)
    % 
    % ---------------------------------------------------
    % s0
    % x(b) = 1.0       v(b) = 3.0
    % x(c) = 5.0       v(c) = 2.0
    % x(d) = 3.0       v(d) = 1.0
    %  b---
    %      c--
    %    d-
    % 
    % ntg(b, c) = 1.33333333333333      via d: 1.33333333333333
    % ttc(b, c) = 4.0      via d: 4.0
    % 
    % ntg(b, d) = 0.666666666666667      via c: 0.666666666666667
    % ttc(b, d) = 1.0      via c: 1.0
    % 
    % ntg(c, d) = -1.0      via b: -1.0
    % ttc(c, d) = -2.0      via b: -2.0
    % 
    % ---------------------------------------------------
    % wait(4.2) do s0
    % x(b) = 13.6       v(b) = 3.0
    % x(c) = 13.4       v(c) = 2.0
    % x(d) = 7.2       v(d) = 1.0
    %               b---
    %              c--
    %        d-
    % 
    % ntg(b, c) = -0.0666666666666667      via d: -0.0666666666666664
    % ttc(b, c) = -0.2      via d: -0.199999999999996
    % 
    % ntg(b, d) = -2.13333333333333      via c: -2.13333333333333
    % ttc(b, d) = -3.2      via c: -3.2
    % 
    % ntg(c, d) = -3.1      via b: -3.1
    % ttc(c, d) = -6.2      via b: -6.2
    % 
    % ---------------------------------------------------
    % accel(d, 4) do wait(4.2) do s0
    % x(b) = 13.6       v(b) = 3.0
    % x(c) = 13.4       v(c) = 2.0
    % x(d) = 7.2       v(d) = 4.0
    %               b---
    %              c--
    %        d----
    % 
    % ntg(b, c) = -0.0666666666666667      via d: -0.0666666666666655
    % ttc(b, c) = -0.2      via d: -0.199999999999998
    % 
    % ntg(b, d) = -2.13333333333333      via c: -2.13333333333333
    % ttc(b, d) = 6.39999999999999      via c: 6.39999999999999
    % 
    % ntg(c, d) = -3.1      via b: -3.1
    % ttc(c, d) = 3.1      via b: 3.1
    % 
    % ---------------------------------------------------
    % wait(3.2) do accel(d, 4) do wait(4.2) do s0
    % x(b) = 23.2       v(b) = 3.0
    % x(c) = 19.8       v(c) = 2.0
    % x(d) = 20.0       v(d) = 4.0
    %                        b---
    %                     c--
    %                     d----
    % 
    % ntg(b, c) = -1.13333333333333      via d: -1.13333333333333
    % ttc(b, c) = -3.4      via d: -3.4
    % 
    % ntg(b, d) = -1.06666666666666      via c: -1.06666666666667
    % ttc(b, d) = 3.19999999999999      via c: 3.19999999999999
    % 
    % ntg(c, d) = 0.1      via b: 0.100000000000003
    % ttc(c, d) = -0.1      via b: -0.100000000000003
    %
    % (End of ECLiPSE-CLP)
    %
test_1(!IO) :-
    check_sit(S0 @ init_sit,
            [test(ntg(b,c), 4.0 `float.'/'` 3.0, "ntg(b,c)")
            ,test(ttc(b,c), 4.0, "ttc(b,c)")
            ,test(ntg(b,d), 2.0 `float.'/'` 3.0, "ntg(b,d)")
            ,test(ttc(b,d), 1.0, "ttc(b,d)")
            ,test(ntg(c,d), -1.0, "ntg(c,d)")
            ,test(ttc(c,d), -2.0, "ttc(c,d)")
            ], !IO),
    check_sit(S1 @ do(wait(4.2), S0),
            [test(ntg(b,c), -2.0 `float.'/'` 30.0, "ntg(b,c)")
            ,test(ttc(b,c), -0.2, "ttc(b,c)")
            ,test(ntg(b,d), -2.133333333333333, "ntg(b,d)")
            ,test(ttc(b,d), -3.2, "ttc(b,d)")
            ,test(ntg(c,d), -3.1, "ntg(c,d)")
            ,test(ttc(c,d), -6.2, "ttc(c,d)")
            ], !IO),
    check_sit(S2 @ do(accel(d, 4.0), S1),
            [test(ntg(b,c), -2.0 `float.'/'` 30.0, "ntg(b,c)")
            ,test(ttc(b,c), -0.2, "ttc(b,c)")
            ,test(ntg(b,d), -2.133333333333333, "ntg(b,d)")
            ,test(ttc(b,d), 6.4, "ttc(b,d)")
            ,test(ntg(c,d), -3.1, "ntg(c,d)")
            ,test(ttc(c,d), 3.1, "ttc(c,d)")
            ], !IO),
    check_sit(_S3 @ do(wait(3.2), S2),
            [test(ntg(b,c), -1.13333333333333, "ntg(b,c)")
            ,test(ttc(b,c), -3.4, "ttc(b,c)")
            ,test(ntg(b,d), -1.06666666666666, "ntg(b,d)")
            ,test(ttc(b,d), 3.2, "ttc(b,d)")
            ,test(ntg(c,d), 0.1, "ntg(c,d)")
            ,test(ttc(c,d), -0.1, "ttc(c,d)")
            ], !IO),
    true.

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.test.
%-----------------------------------------------------------------------------%

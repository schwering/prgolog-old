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
:- pred test_2(io::di, io::uo) is det.
:- pred test_3(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module arithmetic.
:- import_module arithmetic.impl.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type test(N) ---> test_val(func(rstc.sit(N)) = N, N, string)
                ;    test_undef(func(rstc.sit(N)) = N, string).
:- inst test ---> test_val(func(in) = out is semidet, ground, ground)
             ;    test_undef(func(in) = out is semidet, ground).

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
    L = [ (b - info(v(b), 0.0, p(x(b), 1.0)))
        , (c - info(v(c), 0.0, p(x(c), 1.0)))
        , (d - info(v(d), 0.0, p(x(d), 1.0)))
        ].


:- pred check_sit(rstc.sit(float), list(test(float)), io, io).
:- mode check_sit(in, list_skel_in(test), di, uo) is det.

check_sit(_, [], !IO).
check_sit(S, [test_val(Func, Exp, Str) | Rest], !IO) :-
    (   if      float.abs(Func(S) `float.'-'` Exp) `float.'<'` 0.00001
        then    true
        else    write(S, !IO), nl(!IO),
                (   if      some [Val] Val = Func(S)
                    then    format("%s = %f != %f\n", [s(Str), f(Val), f(Exp)], !IO),
                            throw({Str, Val, Exp})
                    else    format("%s = undef != %f\n", [s(Str), f(Exp)], !IO),
                            throw({Str, Exp})
                )
    ),
    check_sit(S, Rest, !IO).
check_sit(S, [test_undef(Func, Str) | Rest], !IO) :-
    (   if      Val = Func(S)
        then    write(S, !IO), nl(!IO),
                format("%s = %f != undef\n", [s(Str), f(Val)], !IO),
                throw({Str, Val})
        else    true
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
            [test_val(ntg(b,c), 4.0 `float.'/'` 3.0, "ntg(b,c)")
            ,test_val(ttc(b,c), 4.0, "ttc(b,c)")
            ,test_val(ntg(b,d), 2.0 `float.'/'` 3.0, "ntg(b,d)")
            ,test_val(ttc(b,d), 1.0, "ttc(b,d)")
            ,test_val(ntg(c,d), -1.0, "ntg(c,d)")
            ,test_val(ttc(c,d), -2.0, "ttc(c,d)")
            ], !IO),
    check_sit(S1 @ do(wait(4.2), S0),
            [test_val(ntg(b,c), -2.0 `float.'/'` 30.0, "ntg(b,c)")
            ,test_val(ttc(b,c), -0.2, "ttc(b,c)")
            ,test_val(ntg(b,d), -2.133333333333333, "ntg(b,d)")
            ,test_val(ttc(b,d), -3.2, "ttc(b,d)")
            ,test_val(ntg(c,d), -3.1, "ntg(c,d)")
            ,test_val(ttc(c,d), -6.2, "ttc(c,d)")
            ], !IO),
    check_sit(S2 @ do(accel(d, 4.0), S1),
            [test_val(ntg(b,c), -2.0 `float.'/'` 30.0, "ntg(b,c)")
            ,test_val(ttc(b,c), -0.2, "ttc(b,c)")
            ,test_val(ntg(b,d), -2.133333333333333, "ntg(b,d)")
            ,test_val(ttc(b,d), 6.4, "ttc(b,d)")
            ,test_val(ntg(c,d), -3.1, "ntg(c,d)")
            ,test_val(ttc(c,d), 3.1, "ttc(c,d)")
            ], !IO),
    check_sit(_S3 @ do(wait(3.2), S2),
            [test_val(ntg(b,c), -1.13333333333333, "ntg(b,c)")
            ,test_val(ttc(b,c), -3.4, "ttc(b,c)")
            ,test_val(ntg(b,d), -1.06666666666666, "ntg(b,d)")
            ,test_val(ttc(b,d), 3.2, "ttc(b,d)")
            ,test_val(ntg(c,d), 0.1, "ntg(c,d)")
            ,test_val(ttc(c,d), -0.1, "ttc(c,d)")
            ], !IO),
    true.

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
    % wait(4.02) do s0
    % x(b) = 13.06       v(b) = 3.0
    % x(c) = 13.04       v(c) = 2.0
    % x(d) = 7.02       v(d) = 1.0
    %              b---
    %              c--
    %        d-
    % 
    % ntg(b, c) = -0.0066666666666666      via d: -0.00666666666666726
    % ttc(b, c) = -0.0199999999999996      via d: -0.0200000000000049
    % 
    % ntg(b, d) = -2.01333333333333      via c: -2.01333333333332
    % ttc(b, d) = -3.02      via c: -3.01999999999997
    % 
    % ntg(c, d) = -3.01      via b: -3.01
    % ttc(c, d) = -6.02      via b: -6.02
    % 
    % ---------------------------------------------------
    % accel(d, 4) do wait(4.02) do s0
    % x(b) = 13.06       v(b) = 3.0
    % x(c) = 13.04       v(c) = 2.0
    % x(d) = 7.02       v(d) = 4.0
    %              b---
    %              c--
    %        d----
    % 
    % ntg(b, c) = -0.0066666666666666      via d: -0.00666666666666638
    % ttc(b, c) = -0.0199999999999996      via d: -0.0199999999999969
    % 
    % ntg(b, d) = -2.01333333333333      via c: -2.01333333333332
    % ttc(b, d) = 6.04      via c: 6.0400000000001
    % 
    % ntg(c, d) = -3.01      via b: -3.01
    % ttc(c, d) = 3.01      via b: 3.01
    % 
    % ---------------------------------------------------
    % wait(3.02) do accel(d, 4) do wait(4.02) do s0
    % x(b) = 22.12       v(b) = 3.0
    % x(c) = 19.08       v(c) = 2.0
    % x(d) = 19.1       v(d) = 4.0
    %                       b---
    %                    c--
    %                    d----
    % 
    % ntg(b, c) = -1.01333333333334      via d: -1.01333333333333
    % ttc(b, c) = -3.04      via d: -3.04
    % 
    % ntg(b, d) = -1.00666666666667      via c: -1.00666666666668
    % ttc(b, d) = 3.02      via c: 3.0200000000001
    % 
    % ntg(c, d) = 0.00999999999999979      via b: 0.00999999999999979
    % ttc(c, d) = -0.00999999999999979      via b: -0.00999999999999934
    % 
    % ---------------------------------------------------
    % wait(1) do wait(3.02) do accel(d, 4) do wait(4.02) do s0
    % x(b) = 25.12       v(b) = 3.0
    % x(c) = 21.08       v(c) = 2.0
    % x(d) = 23.1       v(d) = 4.0
    %                          b---
    %                      c--
    %                        d----
    % 
    % ntg(b, c) = -1.34666666666668      via d: -1.34666666666667
    % ttc(b, c) = -4.04      via d: -4.04
    % 
    % ntg(b, d) = -0.673333333333333      via c: -0.673333333333352
    % ttc(b, d) = 2.02      via c: 2.0200000000001
    % 
    % ntg(c, d) = 1.01      via b: 1.01
    % ttc(c, d) = -1.01      via b: -1.01
    % 
    % ---------------------------------------------------
    % accel(c, 3) do wait(1) do wait(3.02) do accel(d, 4) do wait(4.02) do s0
    % x(b) = 25.12       v(b) = 3.0
    % x(c) = 21.08       v(c) = 6.0
    % x(d) = 23.1       v(d) = 4.0
    %                          b---
    %                      c------
    %                        d----
    % 
    % ntg(b, c) = -1.34666666666668      via d: -1.34666666666667
    % ttc(b, c) = 1.3466666666667      via d: 1.34666666666667
    % 
    % ntg(b, d) = -0.673333333333333      via c: -0.673333333333352
    % ttc(b, d) = 2.02      via c: 2.0200000000001
    % 
    % ntg(c, d) = 0.336666666666667      via b: 0.336666666666667
    % ttc(c, d) = 1.01      via b: 1.01
    % 
    % ---------------------------------------------------
    % wait(1) do accel(c, 3) do wait(1) do wait(3.02) do accel(d, 4) do wait(4.02) do s0
    % x(b) = 28.12       v(b) = 3.0
    % x(c) = 27.08       v(c) = 6.0
    % x(d) = 27.1       v(d) = 4.0
    %                             b---
    %                            c------
    %                            d----
    % 
    % ntg(b, c) = -0.346666666666693      via d: -0.346666666666666
    % ttc(b, c) = 0.346666666666696      via d: 0.346666666666666
    % 
    % ntg(b, d) = -0.34      via c: -0.340000000000026
    % ttc(b, d) = 1.02      via c: 1.02000000000009
    % 
    % ntg(c, d) = 0.00333333333333324      via b: 0.00333333333333319
    % ttc(c, d) = 0.00999999999999979      via b: 0.00999999999999945
    % 
    % (End of ECLiPSE-CLP)
    %
test_2(!IO) :-
    check_sit(S0 @ init_sit,
            [test_val(ntg(b,c), 1.33333333333333, "ntg(b,c)")
            ,test_val(ttc(b,c), 4.0, "ttc(b,c)")
            ,test_val(ntg(b,d), 0.66666666666666, "ntg(b,d)")
            ,test_val(ttc(b,d), 1.0, "ttc(b,d)")
            ,test_val(ntg(c,d), -1.0, "ntg(c,d)")
            ,test_val(ttc(c,d), -2.0, "ttc(c,d)")
            ], !IO),
    check_sit(S1 @ do(wait(4.02), S0),
            [test_val(ntg(b,c), -0.0066666666666666, "ntg(b,c)")
            ,test_val(ttc(b,c), -0.0199999999999999, "ttc(b,c)")
            ,test_val(ntg(b,d), -2.0133333333333333, "ntg(b,d)")
            ,test_val(ttc(b,d), -3.02, "ttc(b,d)")
            ,test_val(ntg(c,d), -3.01, "ntg(c,d)")
            ,test_val(ttc(c,d), -6.02, "ttc(c,d)")
            ], !IO),
    check_sit(S2 @ do(accel(d, 4.0), S1),
            [test_val(ntg(b,c), -0.00666666666, "ntg(b,c)")
            ,test_val(ttc(b,c), -0.019999999999, "ttc(b,c)")
            ,test_val(ntg(b,d), -2.0133333333333333, "ntg(b,d)")
            ,test_val(ttc(b,d), 6.04, "ttc(b,d)")
            ,test_val(ntg(c,d), -3.01, "ntg(c,d)")
            ,test_val(ttc(c,d), 3.01, "ttc(c,d)")
            ], !IO),
    check_sit(S3 @ do(wait(3.02), S2),
            [test_val(ntg(b,c), -1.01333333333333, "ntg(b,c)")
            ,test_val(ttc(b,c), -3.04, "ttc(b,c)")
            ,test_val(ntg(b,d), -1.00666666666666, "ntg(b,d)")
            ,test_val(ttc(b,d), 3.02, "ttc(b,d)")
            ,test_val(ntg(c,d), 0.009999999999999, "ntg(c,d)")
            ,test_val(ttc(c,d), -0.00999999999999, "ttc(c,d)")
            ], !IO),
    check_sit(S4 @ do(wait(1.0), S3),
            [test_val(ntg(b,c), -1.3466666666666, "ntg(b,c)")
            ,test_val(ttc(b,c), -4.04, "ttc(b,c)")
            ,test_val(ntg(b,d), -0.6733333333333, "ntg(b,d)")
            ,test_val(ttc(b,d), 2.02, "ttc(b,d)")
            ,test_val(ntg(c,d), 1.01, "ntg(c,d)")
            ,test_val(ttc(c,d), -1.01, "ttc(c,d)")
            ], !IO),
    check_sit(S5 @ do(accel(c, 3.0), S4),
            [test_val(ntg(b,c), -1.346666666666, "ntg(b,c)")
            ,test_val(ttc(b,c), 1.346666666666, "ttc(b,c)")
            ,test_val(ntg(b,d), -0.673333333333, "ntg(b,d)")
            ,test_val(ttc(b,d), 2.02, "ttc(b,d)")
            ,test_val(ntg(c,d), 0.33666666666, "ntg(c,d)")
            ,test_val(ttc(c,d), 1.01, "ttc(c,d)")
            ], !IO),
    check_sit(_S6 @ do(wait(1.0), S5),
            [test_val(ntg(b,c), -0.346666666666, "ntg(b,c)")
            ,test_val(ttc(b,c), 0.346666666666, "ttc(b,c)")
            ,test_val(ntg(b,d), -0.34, "ntg(b,d)")
            ,test_val(ttc(b,d), 1.02, "ttc(b,d)")
            ,test_val(ntg(c,d), 0.0033333333333, "ntg(c,d)")
            ,test_val(ttc(c,d), 0.0099999999999, "ttc(c,d)")
            ], !IO),
    true.


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
    % wait(4.0) do s0
    % x(b) = 13.0       v(b) = 3.0
    % x(c) = 13.0       v(c) = 2.0
    % x(d) = 7.0       v(d) = 1.0
    %              b---
    %              c--
    %        d-
    % 
    % ntg(b, c) = 0.0      via d: 0.0
    % ttc(b, c) = 0.0      via d: 3.5527136788005e-15
    % 
    % ntg(b, d) = -2.0
    % ttc(b, d) = -3.0
    % 
    % ntg(c, d) = -3.0
    % ttc(c, d) = -6.0
    % 
    % ---------------------------------------------------
    % accel(d, 4) do wait(4.0) do s0
    % x(b) = 13.0       v(b) = 3.0
    % x(c) = 13.0       v(c) = 2.0
    % x(d) = 7.0       v(d) = 4.0
    %              b---
    %              c--
    %        d----
    % 
    % ntg(b, c) = 0.0      via d: 0.0
    % ttc(b, c) = 0.0      via d: 0.0
    % 
    % ntg(b, d) = -2.0
    % ttc(b, d) = 6.0
    % 
    % ntg(c, d) = -3.0
    % ttc(c, d) = 3.0
    % 
    % ---------------------------------------------------
    % wait(3.0) do accel(d, 4) do wait(4.0) do s0
    % x(b) = 22.0       v(b) = 3.0
    % x(c) = 19.0       v(c) = 2.0
    % x(d) = 19.0       v(d) = 4.0
    %                       b---
    %                    c--
    %                    d----
    % 
    % ntg(b, c) = -1.0      via d: -1.0
    % ttc(b, c) = -3.0
    % 
    % ntg(b, d) = -1.0      via c: -1.0
    % ttc(b, d) = 3.0
    % 
    % ntg(c, d) = 0.0
    % ttc(c, d) = 0.0
    % 
    % ---------------------------------------------------
    % wait(1) do wait(3.0) do accel(d, 4) do wait(4.0) do s0
    % x(b) = 25.0       v(b) = 3.0
    % x(c) = 21.0       v(c) = 2.0
    % x(d) = 23.0       v(d) = 4.0
    %                          b---
    %                      c--
    %                        d----
    % 
    % ntg(b, c) = -1.33333333333333      via d: -1.33333333333333
    % ttc(b, c) = -4.0      via d: -4.0
    % 
    % ntg(b, d) = -0.666666666666667
    % ttc(b, d) = 2.0
    % 
    % ttc(c, d) = -1.0
    % 
    % ---------------------------------------------------
    % accel(c, 3) do wait(1) do wait(3.0) do accel(d, 4) do wait(4.0) do s0
    % x(b) = 25.0       v(b) = 3.0
    % x(c) = 21.0       v(c) = 6.0
    % x(d) = 23.0       v(d) = 4.0
    %                          b---
    %                      c------
    %                        d----
    % 
    % ntg(b, c) = -1.33333333333333      via d: -1.33333333333333
    % ttc(b, c) = 1.33333333333333      via d: 1.33333333333333
    % 
    % ntg(b, d) = -0.666666666666667
    % ttc(b, d) = 2.0
    % 
    % ---------------------------------------------------
    % wait(1) do accel(c, 3) do wait(1) do wait(3.0) do accel(d, 4) do wait(4.0) do s0
    % x(b) = 28.0       v(b) = 3.0
    % x(c) = 27.0       v(c) = 6.0
    % x(d) = 27.0       v(d) = 4.0
    %                             b---
    %                            c------
    %                            d----
    % 
    % ntg(b, c) = -0.333333333333333      via d: -0.333333333333333
    % ttc(b, c) = 0.333333333333333
    % 
    % ntg(b, d) = -0.333333333333333
    % ttc(b, d) = 1.0
    % 
    % 
    % (End of ECLiPSE-CLP)
    %
test_3(!IO) :-
    check_sit(S0 @ init_sit,
            [test_val(ntg(b,c), 1.33333333333333, "ntg(b,c)")
            ,test_val(ttc(b,c), 4.0, "ttc(b,c)")
            ,test_val(ntg(b,d), 0.66666666666666, "ntg(b,d)")
            ,test_val(ttc(b,d), 1.0, "ttc(b,d)")
            ,test_val(ntg(c,d), -1.0, "ntg(c,d)")
            ,test_val(ttc(c,d), -2.0, "ttc(c,d)")
            ], !IO),
    check_sit(S1 @ do(wait(4.0), S0),
            [test_val(ntg(b,c), 0.0, "ntg(b,c)")
            ,test_val(ttc(b,c), 0.0, "ttc(b,c)")
            ,test_val(ntg(b,d), -2.0, "ntg(b,d)")
            ,test_val(ttc(b,d), -3.0, "ttc(b,d)")
            ,test_val(ntg(c,d), -3.0, "ntg(c,d)")
            ,test_val(ttc(c,d), -6.0, "ttc(c,d)")
            ], !IO),
    check_sit(S2 @ do(accel(d, 4.0), S1),
            [test_val(ntg(b,c), 0.0, "ntg(b,c)")
            ,test_val(ttc(b,c), 0.0, "ttc(b,c)")
            ,test_val(ntg(b,d), -2.0, "ntg(b,d)")
            ,test_val(ttc(b,d), 6.0, "ttc(b,d)")
            ,test_val(ntg(c,d), -3.0, "ntg(c,d)")
            ,test_val(ttc(c,d), 3.0, "ttc(c,d)")
            ], !IO),
    check_sit(S3 @ do(wait(3.0), S2),
            [test_val(ntg(b,c), -1.0, "ntg(b,c)")
            ,test_val(ttc(b,c), -3.0, "ttc(b,c)")
            ,test_val(ntg(b,d), -1.0, "ntg(b,d)")
            ,test_val(ttc(b,d), 3.0, "ttc(b,d)")
            ,test_val(ntg(c,d), 0.0, "ntg(c,d)")
            ,test_val(ttc(c,d), 0.0, "ttc(c,d)")
            ], !IO),
    check_sit(S4 @ do(wait(1.0), S3),
            [test_val(ntg(b,c), -1.3333333333333, "ntg(b,c)")
            ,test_val(ttc(b,c), -4.0, "ttc(b,c)")
            ,test_val(ntg(b,d), -0.6666666666666, "ntg(b,d)")
            ,test_val(ttc(b,d), 2.0, "ttc(b,d)")
            ,test_undef(ntg(c,d), "ntg(c,d)")
            ,test_val(ttc(c,d), -1.0, "ttc(c,d)")
            ], !IO),
    check_sit(S5 @ do(accel(c, 3.0), S4),
            [test_val(ntg(b,c), -1.3333333333, "ntg(b,c)")
            ,test_val(ttc(b,c), 1.3333333333, "ttc(b,c)")
            ,test_val(ntg(b,d), -0.6666666666, "ntg(b,d)")
            ,test_val(ttc(b,d), 2.0, "ttc(b,d)")
            ,test_undef(ntg(c,d), "ntg(c,d)")
            ,test_undef(ttc(c,d), "ttc(c,d)")
            ], !IO),
    check_sit(_S6 @ do(wait(1.0), S5),
            [test_val(ntg(b,c), -0.33333333333, "ntg(b,c)")
            ,test_val(ttc(b,c), 0.33333333333, "ttc(b,c)")
            ,test_val(ntg(b,d), -0.3333333333, "ntg(b,d)")
            ,test_val(ttc(b,d), 1.0, "ttc(b,d)")
            ,test_undef(ntg(c,d), "ntg(c,d)")
            ,test_undef(ttc(c,d), "ttc(c,d)")
            ], !IO),
    true.


%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.test.
%-----------------------------------------------------------------------------%

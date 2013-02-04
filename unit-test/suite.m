%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: suite.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module suite.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type test_pred == (pred(io, io)).
:- inst test_pred == (pred(di, uo) is cc_multi).

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module string.

:- use_module util.
:- use_module util.arithmetic.
:- use_module util.arithmetic.impl.
:- use_module util.arithmetic.impl.test.
:- use_module util.inf_arithmetic.
:- use_module util.inf_arithmetic.test.
:- use_module util.pso.
:- use_module util.pso.test.
:- use_module util.simulated_annealing.
:- use_module util.simulated_annealing.test.
:- use_module util.rand.
:- use_module util.rand.test.

:- use_module prgolog.
:- use_module prgolog.test.
:- use_module tree.
:- use_module tree.test.

:- use_module domain.
:- use_module domain.car.
:- use_module domain.car.rstc.
:- use_module domain.car.rstc.test.
:- use_module domain.car.rstc.bat.
:- use_module domain.car.rstc.bat.test.
:- use_module domain.car.rstc.fuzzy.
:- use_module domain.car.rstc.fuzzy.test.

%-----------------------------------------------------------------------------%

:- type test ---> test(mod       :: string,
                       name      :: string,
                       predicate :: test_pred).
:- inst test ---> test(ground, ground, test_pred).

%-----------------------------------------------------------------------------%

/*
:- mutable(tests, list(test), [], list(test), [attach_to_io_state, untrailed]).

:- pred register_test(string::in, string::in, test_pred::in(test_pred),
                      io::di, io::uo) is det.

register_test(Module, Pred, Test, !IO) :-
    get_tests(Tests, !IO),
    Tests1 = [test(Module, Pred, Test) | Tests],
    set_tests(Tests1, !IO).
*/

:- inst det_test_pred == (pred(di, uo) is det).
:- inst multi_test_pred == (pred(di, uo) is multi).

:- func ccm(test_pred) = test_pred.
:- mode ccm(in(test_pred)) = out(test_pred) is det.
:- mode ccm(in(det_test_pred)) = out(test_pred) is det.
:- mode ccm(in(multi_test_pred)) = out(test_pred) is det.

ccm(P) = ( pred(IO0::di, IO1::uo) is cc_multi :- P(IO0, IO1) ).

:- pred get_tests(list(test)::out(list(test)), io::di, io::uo) is det.

get_tests(Tests, !IO) :-
    Tests = [ test("tree", "test_force", ccm(tree.test.test_force))
            , test("tree", "test_search_det", ccm(tree.test.test_search_det))
            , test("tree", "test_search_semidet", ccm(tree.test.test_search_semidet))
            , test("prgolog", "test_next", ccm(prgolog.test.test_next))
            , test("prgolog", "test_next2", ccm(prgolog.test.test_next2))
            , test("prgolog", "test_final", ccm(prgolog.test.test_final))
            , test("prgolog", "test_value", ccm(prgolog.test.test_value))
            , test("prgolog", "test_trans_atom", ccm(prgolog.test.test_trans_atom))
            , test("prgolog", "test_trans", ccm(prgolog.test.test_trans))
            , test("prgolog", "test_final2", ccm(prgolog.test.test_final2))
            , test("rstc", "test_1", ccm(domain.car.rstc.test.test_1))
            , test("rstc", "test_2", ccm(domain.car.rstc.test.test_2))
            , test("rstc", "test_3", ccm(domain.car.rstc.test.test_3))
            %, test("bat", "test_ntg", ccm(domain.car.rstc.bat.test.test_ntg))
            %, test("bat", "test_ttc", ccm(domain.car.rstc.bat.test.test_ttc))
            , test("bat", "test_match_dist", ccm(domain.car.rstc.bat.test.test_match_dist))
            , test("fuzzy", "test_ntg", ccm(domain.car.rstc.fuzzy.test.test_ntg))
            , test("fuzzy", "test_ttc", ccm(domain.car.rstc.fuzzy.test.test_ttc))
            , test("arithmetic", "test_bin_search_rat", ccm(util.arithmetic.impl.test.test_bin_search_rat))
            , test("arithmetic", "test_bin_search_rat_sq", ccm(util.arithmetic.impl.test.test_bin_search_rat_sq))
            , test("arithmetic", "test_bin_search_float", ccm(util.arithmetic.impl.test.test_bin_search_float))
            , test("arithmetic", "test_bin_search_float_inv", ccm(util.arithmetic.impl.test.test_bin_search_float_inv))
            , test("arithmetic", "test_bin_search_float_sq", ccm(util.arithmetic.impl.test.test_bin_search_float_sq))
            , test("arithmetic", "test_bin_search_float_sq2", ccm(util.arithmetic.impl.test.test_bin_search_float_sq2))
            , test("arithmetic", "test_bin_search_float_sqrt", ccm(util.arithmetic.impl.test.test_bin_search_float_sqrt))
            , test("arithmetic", "test_bin_search_float_fail", ccm(util.arithmetic.impl.test.test_bin_search_float_fail))
            , test("arithmetic", "test_optimize_float", ccm(util.arithmetic.impl.test.test_optimize_float))
            , test("inf_arithmetic", "test", ccm(util.inf_arithmetic.test.test))
            , test("pso", "test_sa", ccm(util.pso.test.test_pso))
            , test("rand", "test_random1", ccm(util.rand.test.test_random1))
            , test("rand", "test_random2", ccm(util.rand.test.test_random2))
            , test("simulated_annealing", "test_sa", ccm(util.simulated_annealing.test.test_sa))
            ].

%-----------------------------------------------------------------------------%

:- pred run_test(test::in(test), io::di, io::uo) is cc_multi.

run_test(Test, !IO) :-
    Test = test(Mod, Name, TestPred),
    format("Running test %s / %s\n", [s(Mod), s(Name)], !IO),
    try_io((pred(R::out, IO0::di, IO1::uo) is cc_multi :-
        TestPred(IO0, IO1),
        R = 0 % some dummy value
    ), Result, !IO),
    (   Result = succeeded(_)
    ;   Result = exception(Exc),
        format("!!! Exception in test %s / %s: ", [s(Mod), s(Name)], !IO),
        write(Exc, !IO),
        nl(!IO)
    ).

main(!IO) :-
    get_tests(Tests, !IO),
    %format("Running tests.\n", [], !IO),
    foldl(run_test, Tests, !IO),
    %format("Done!\n", [], !IO),
    true.

%-----------------------------------------------------------------------------%
:- end_module suite.
%-----------------------------------------------------------------------------%

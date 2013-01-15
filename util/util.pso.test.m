%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: util.pso.test.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module util.pso.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test_random(io::di, io::uo) is det.
:- pred test_pso(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module list.
:- import_module math.
:- import_module string.
:- import_module util.rat.
:- import_module require.
:- import_module std_util.

%-----------------------------------------------------------------------------%

test_random(!IO) :-
    some [!RandomSupply] (
        random.init(10, !:RandomSupply),
        some [Lo, Hi, X] ( {Lo, Hi} = {0.0, 1.0}, random({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {-10.0, 1.0}, random({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {-1.0, 1.0}, random({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {-1.0, 0.0}, random({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {50.0, 100.0}, random({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {50.0, 50.0}, random({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) )),
        some [Lo, Hi, X] ( {Lo, Hi} = {-50.0, -50.0}, random({Lo, Hi}, X, !RandomSupply), ( if Lo =< X, X =< Hi then true else throw({Lo, X, Hi}) ))
    ),
    true.


test_pso(!IO) :-
%   Just a test whether Mercury optimizes-away duplicate higher-order
%   function calls (result: yes iff compiled with --optimize-duplicate-calls):
%   Func = (func(X0) = X1 is semidet :- (
%       trace [io(!IO)] ( format("Func(%f) = ?\n", [f(X0)], !IO) ),
%       F = (func(Z) = Z*Z*Z*Z - 5.0 * X0 * Z*Z + 4.0 * X0),
%       random.init(10, RandomSupply),
%       run_pso(5, 1000, default_params, {-1000.0, 1000.0}, min, F, ordering, X1, RandomSupply, _),
%       %XX = -1.58113883,
%       XX = 1.58113883,
%       Eps = 0.001,
%       XX-Eps =< X1, X1 =< XX+Eps
%   )),
%   ( if Bla = ( if Func(1.0) >= -20.0 then Func(1.0) else 10000.0 ) then
%       format("Bla = %f\n", [f(Bla)], !IO)
%     else true ),

    some [!RandomSupply] (
        random.init(10, !:RandomSupply),
        some [F, X, Eps, X0, X1] (
            F = (func(X) = X*X*X*X - 5.0 * X*X + 4.0),
            run_pso(5, 1000, default_params, {-1000.0, 1000.0}, min, F, ordering, X, !RandomSupply),
            X0 = -1.58113883,
            X1 = 1.58113883,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps ; X1-Eps =< X, X =< X1+Eps then true else throw({"F1", X0, X1, X}) )
        ),
        some [F, X, Eps, X0] (
            F = (func(X) = 0.2 * X*X*X*X*X - 5.0 * X*X*X + 4.0 * X),
            run_pso(5, 100, default_params, {-1000.0, 5.0}, max, F, ordering, X, !RandomSupply),
            X0 = -3.837761867,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F2", X0, X}) )
        ),
        some [F, X, Eps, X0] (
            F = abs,
            run_pso(5, 10000, default_params, {-1000.0, 1000.0}, min, F, ordering, X, !RandomSupply),
            X0 = 0.0,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F3.1", X0, X}) )
        ),
        some [F, X, Eps, X0] (
            F = abs,
            run_pso(5, 100, default_params, {-1000.0, 1000.0}, min, F, ordering, X, !RandomSupply),
            X0 = 0.0,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F3.2", X0, X}) )
        ),
        some [F, X, Eps, X0] (
            F = abs,
            run_pso(5, 50, default_params, {-2.0, 2.0}, min, F, ordering, X, !RandomSupply),
            X0 = 0.0,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F3.3", X0, X}) )
        ),
        % This one fails if the lower bound is less than 0.0 (see next one for reason):
        some [F, X, Eps, X0] (
            F = (func(X) = ( if X =< 0.0 then 1.0 else max(-1.0 * ln(X), ln(X)) )),
            run_pso(5, 10000, default_params, {0.0, 1000.0}, min, F, ordering, X, !RandomSupply),
            X0 = 1.0,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F4", X0, X}) )
        ),
        % This one fails because the probability to hit the small minimal bump
        % is close to zero. Since probably no particle will hit it, the swarm
        % doesn't know where to go. Well I guess there is no algorithm to find
        % such things, right?
%       some [F, X, Eps, X0] (
%           F = (func(X) = ( if abs(X) > 0.00001 then 1.0 else 0.0 )),
%           run_pso(5, 1000, default_params, {0.0, 1000.0}, min, F, ordering, X, !RandomSupply),
%           X0 = 0.0,
%           Eps = 0.001,
%           ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F5", X0, X}) )
%       ),
        true
    ),
    true.

%-----------------------------------------------------------------------------%
:- end_module util.pso.test.
%-----------------------------------------------------------------------------%

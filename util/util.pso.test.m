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

:- pred test_pso(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

    % This submodule helps to avoid ambiguous overloads due to the imports
    % of vector_space.impl.
:- module sub.
    :- interface.

    :- pred run_pso(num_particles::in, num_iterations::in, params::in,
                    bounds(float)::in,
                    direction::in, objective_func(float, U)::in, comparison_func(U)::in,
                    best_global_position(float)::out,
                    random_supply::mdi, random_supply::muo) is det.

    :- implementation.

    :- import_module util.vector_space.
    :- import_module util.vector_space.impl.

    run_pso(M, N, Params, Bounds, Direction, F, Cmp, G, !RandomSupply) :-
        pso.run_pso(M, N, Params, Bounds, Direction, F, Cmp, G, !RandomSupply).

:- end_module sub.

%-----------------------------------------------------------------------------%

:- import_module bool.
:- import_module exception.
:- import_module list.
:- import_module math.
:- import_module string.
:- import_module require.
:- import_module std_util.
:- import_module util.pso.test.sub.

%-----------------------------------------------------------------------------%


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
        init(10, !:RandomSupply),
        some [F, X, Eps, X0, X1] (
            F = (func(Z) = Z*Z*Z*Z - 5.0 * Z*Z + 4.0),
            sub.run_pso(5, 1000, default_params, {-1000.0, 1000.0}, min, F, ordering, X, !RandomSupply),
            X0 = -1.58113883,
            X1 = 1.58113883,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps ; X1-Eps =< X, X =< X1+Eps then true else throw({"F1", X0, X1, X}) )
        ),
        some [F, X, Eps, X0] (
            F = (func(Z) = 0.2 * Z*Z*Z*Z*Z - 5.0 * Z*Z*Z + 4.0 * Z),
            sub.run_pso(5, 100, default_params, {-1000.0, 5.0}, max, F, ordering, X, !RandomSupply),
            X0 = -3.837761867,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F2", X0, X}) )
        ),
        some [F, X, Eps, X0] (
            F = float.abs,
            sub.run_pso(5, 10000, default_params, {-1000.0, 1000.0}, min, F, ordering, X, !RandomSupply),
            X0 = 0.0,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F3.1", X0, X}) )
        ),
        some [F, X, Eps, X0] (
            F = float.abs,
            sub.run_pso(5, 100, default_params, {-1000.0, 1000.0}, min, F, ordering, X, !RandomSupply),
            X0 = 0.0,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F3.2", X0, X}) )
        ),
        some [F, X, Eps, X0] (
            F = float.abs,
            sub.run_pso(5, 50, default_params, {-2.0, 2.0}, min, F, ordering, X, !RandomSupply),
            X0 = 0.0,
            Eps = 0.001,
            ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F3.3", X0, X}) )
        ),
        % This one fails if the lower bound is less than 0.0 (see next one for reason):
%       some [F, X, Eps, X0] (
%           F = (func(Z) = ( if Z =< 0.0 then 1.0 else max(-1.0 * ln(Z), ln(Z)) )),
%           sub.run_pso(5, 10000, default_params, {0.0, 1000.0}, min, F, ordering, X, !RandomSupply),
%           X0 = 1.0,
%           Eps = 0.001,
%           ( if X0-Eps =< X, X =< X0+Eps then true else throw({"F4", X0, X}) )
%       ),
        % This one fails because the probability to hit the small minimal bump
        % is close to zero. Since probably no particle will hit it, the swarm
        % doesn't know where to go. Well I guess there is no algorithm to find
        % such things, right?
%       some [F, X, Eps, X0] (
%           F = (func(Z) = ( if abs(Z) > 0.00001 then 1.0 else 0.0 )),
%           sub.run_pso(5, 1000, default_params, {0.0, 1000.0}, min, F, ordering, X, !RandomSupply),
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

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: util.pso.m.
% Main author: schwering.
%
% Partical swarm optimization.
%
% We adhere to the naming conventions used by the English Wikipedia article
% (among others):
%  - X stands for the current position of a certain particle,
%  - P stands for the optimal position of a certain particle found thus far,
%  - V stands for the current velocity of a certain particle,
%  - G stands for the optimal position of all particles found thus far.
%
%-----------------------------------------------------------------------------%

:- module util.pso.

:- interface.

:- import_module util.rand.
:- import_module util.vector_space.

%-----------------------------------------------------------------------------%

:- type num_particles == int.
:- type num_iterations == int.

:- type params ---> params(inertia_weight  :: float,
                           cognitive_param :: float,
                           social_param    :: float).

:- type direction ---> min ; max.

:- type objective_func(T, U) == (func(T) = U).

:- type particles(T).

:- type best_global_position(T) == T.

%-----------------------------------------------------------------------------%

:- func default_params = params.

:- pred init_pso(num_particles::in, bounds(T)::in,
                 objective_func(T, U)::in, comparison_func(U)::in,
                 particles(T)::out, best_global_position(T)::out,
                 random_supply::mdi, random_supply::muo) is det
    <= (random_generator(T), vector_space(T)).

:- pred pso(params, bounds(T),
            direction, objective_func(T, U), comparison_func(U),
            particles(T), particles(T),
            best_global_position(T), best_global_position(T),
            random_supply, random_supply)
    <= (random_generator(T), vector_space(T)).
:- mode pso(in, in,
            in, in, in,
            in, out,
            in, out,
            mdi, muo) is det.
%:- mode pso(in, in,
%            in, in, in,
%            in, out,
%            in, out,
%            in, out) is det.

%-----------------------------------------------------------------------------%

    % run_pso(M, N, Params, Bounds, Direction, F, G, !RandomSupply):
    % Creates a swarm of M particles and then runs N iterations of optimization.
    % G is the optimal found value (G for global).
    % The random generator is seeded with the product of M, N and all numbers in
    % Params.
    %
:- pred run_pso(num_particles::in, num_iterations::in, params::in,
                bounds(T)::in,
                direction::in, objective_func(T, U)::in, comparison_func(U)::in,
                best_global_position(T)::out) is det
    <= (random_generator(T), vector_space(T)).

:- pred run_pso(num_particles::in, num_iterations::in, params::in,
                bounds(T)::in,
                direction::in, objective_func(T, U)::in, comparison_func(U)::in,
                best_global_position(T)::out,
                random_supply::mdi, random_supply::muo) is det
    <= (random_generator(T), vector_space(T)).

%-----------------------------------------------------------------------------%

:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.
:- import_module list.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type particle(T) ---> particle(position      :: T,
                                  velocity      :: T,
                                  best_position :: T).

:- type particles(T) == list(particle(T)).

%-----------------------------------------------------------------------------%

default_params = params(0.729, 1.49445, 1.49445).

%-----------------------------------------------------------------------------%

init_pso(M, Bounds @ {Lo, Hi}, F, Cmp, Particles, G, !RandomSupply) :-
    Part = particle(X, V, P),
    random({Lo, Hi}, P, !RandomSupply),
    random({-1.0 `sc_mult` abs(Hi `minus` Lo),
                        abs(Hi `minus` Lo)}, V, !RandomSupply),
    P = X,
    (   if      M =< 0
        then    unexpected($module, $pred, "non-positive number of particles")
        else if M = 1
        then    Particles = [Part],
                G = P
        else    init_pso(M-1, Bounds, F, Cmp, Rest, G0, !RandomSupply),
                Particles = [Part|Rest],
                G = ( if Cmp(F(P), F(G0)) = (>) then P else G0 )
    ).

%-----------------------------------------------------------------------------%

pso(Params, Bounds, min, F, Cmp, !Particles, !G, !RandomSupply) :-
    Cmp1 = (func(X, Y) = Res1 is det :-
        Res = Cmp(X, Y),
        ( Res = (>), Res1 = (<)
        ; Res = (=), Res1 = (=)
        ; Res = (<), Res1 = (>) )
    ),
    pso(Params, Bounds, max, F, Cmp1, !Particles, !G, !RandomSupply).
pso(Params, Bounds, max, F, Cmp, !Particles, !G, !RandomSupply) :-
    map_foldl2(update(Params, Bounds, F, Cmp), !Particles, !G, !RandomSupply).


:- pred update(params, bounds(T),
               objective_func(T, U), comparison_func(U),
               particle(T), particle(T),
               best_global_position(T), best_global_position(T),
               random_supply, random_supply)
    <= (random_generator(T), vector_space(T)).
:- mode update(in, in,
               in, in,
               in, out,
               in, out,
               mdi, muo) is det.
%:- mode update(in, in,
%               in, in,
%               in, out,
%               in, out,
%               in, out) is det.

update(params(InertiaWeight, CognitiveParam, SocialParam), {Lo, Hi}, F, Cmp,
       particle(X1, V1, P1), particle(X2, V2, P2),
       G1, G2,
       !RandomSupply) :-
    random_float({0.0, 1.0}, RP, !RandomSupply),
    random_float({0.0, 1.0}, RG, !RandomSupply),
    V2 = (InertiaWeight `sc_mult` V1) `plus`
         ((CognitiveParam * RP) `sc_mult` (P1 `minus` X1)) `plus`
         ((SocialParam * RG) `sc_mult` (G1 `minus` X1)),
    X2 = X1 `plus` V2,
    (   if      Lo `leq` X2, X2 `leq` Hi, Cmp(F(X2), F(P1)) = (>)
        then    P2 = X2,
                G2 = ( if Cmp(F(X2), F(G1)) = (>) then X2 else G1 )
        else    P2 = P1,
                G2 = G1
    ).

%-----------------------------------------------------------------------------%

run_pso(M, N, Params, Bounds, Direction, F, Cmp, G) :-
    Seed = M * N * floor_to_int(inertia_weight(Params) *
                                cognitive_param(Params) *
                                social_param(Params)),
    init(Seed, RandomSupply),
    run_pso(M, N, Params, Bounds, Direction, F, Cmp, G, RandomSupply, _).


run_pso(M, N, Params, Bounds, Direction, F, Cmp, G, !RandomSupply) :-
    init_pso(M, Bounds, F, Cmp, Particles, G0, !RandomSupply),
    iterate_pso(N, Params, Bounds, Direction, F, Cmp,
                Particles, _,
                G0, G,
                !RandomSupply).


:- pred iterate_pso(num_particles::in, params::in, bounds(T)::in, direction::in,
                    objective_func(T, U)::in, comparison_func(U)::in,
                    particles(T)::in, particles(T)::out,
                    best_global_position(T)::in, best_global_position(T)::out,
                    random_supply::mdi, random_supply::muo) is det
    <= (random_generator(T), vector_space(T)).

iterate_pso(N, Params, Bounds, Direction, F, Cmp,
            !Particles, !G, !RandomSupply) :-
    if      N =< 0
    then    true
    else    pso(Params, Bounds, Direction, F, Cmp, !Particles, !G,
                !RandomSupply),
            iterate_pso(N-1, Params, Bounds, Direction, F, Cmp, !Particles, !G,
                        !RandomSupply).

%-----------------------------------------------------------------------------%
:- end_module util.pso.
%-----------------------------------------------------------------------------%

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

:- use_module random.

%-----------------------------------------------------------------------------%

:- type params ---> params(inertia_weight  :: float,
                           cognitive_param :: float,
                           social_param    :: float).

:- type bounds == {float, float}.

:- type direction ---> min ; max.

:- type particles.

:- type best_global_position == float.

%-----------------------------------------------------------------------------%

:- func default_params = params.

:- pred init_pso(int::in, bounds::in, (func(float) = float)::in,
                 particles::out, best_global_position::out,
                 random.supply::mdi, random.supply::muo) is det.

:- pred pso(params::in, bounds::in, direction::in, (func(float) = float)::in,
            particles::in, particles::out,
            best_global_position::in, best_global_position::out,
            random.supply::mdi, random.supply::muo) is det.

%-----------------------------------------------------------------------------%

    % run_pso(M, N, Params, Bounds, Direction, F, G, !RandomSupply):
    % Creates a swarm of M particles and then runs N iterations of optimization.
    % G is the optimal found value (G for global).
    %
:- pred run_pso(int::in, int::in, params::in, bounds::in, direction::in,
                (func(float) = float)::in, best_global_position::out,
                random.supply::mdi, random.supply::muo) is det.

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

:- type particle ---> particle(position      :: float,
                               velocity      :: float,
                               best_position :: float).

:- type particles == list(particle).

%-----------------------------------------------------------------------------%

default_params = params(0.729, 1.49445, 1.49445).

%-----------------------------------------------------------------------------%

:- pred random(float::out, random.supply::mdi, random.supply::muo) is det.

random(Outcome, !RandomSupply) :-
    random.random(Numer, !RandomSupply),
    random.randmax(Denom, !RandomSupply),
    Outcome = float(Numer) / float(Denom).


:- pred random(bounds::in, float::out,
               random.supply::mdi, random.supply::muo) is det.

random({Lo, Hi}, Outcome, !RandomSupply) :-
    random(Outcome0, !RandomSupply),
    Outcome = Outcome0 * (Hi - Lo) + Lo.

%-----------------------------------------------------------------------------%

init_pso(M, Bounds @ {Lo, Hi}, F, Particles, G, !RandomSupply) :-
    Part = particle(X, V, P),
    random(Bounds, P, !RandomSupply),
    random({-1.0 * abs(Hi - Lo), abs(Hi - Lo)}, V, !RandomSupply),
    P = X,
    (   if      M =< 0
        then    unexpected($module, $pred, "non-positive number of particles")
        else if M = 1
        then    Particles = [Part],
                G = P
        else    init_pso(M-1, Bounds, F, Rest, G0, !RandomSupply),
                Particles = [Part|Rest],
                G = ( if F(P) >= F(G0) then P else G0 )
    ).

%-----------------------------------------------------------------------------%

pso(Params, Bounds, min, F, !Particles, !G, !RandomSupply) :-
    G = (func(X) = -1.0 * F(X)),
    pso(Params, Bounds, max, G, !Particles, !G, !RandomSupply).
pso(Params, Bounds, max, F, !Particles, !G, !RandomSupply) :-
    map_foldl2(update(Params, Bounds, F), !Particles, !G, !RandomSupply).


:- pred update(params::in, bounds::in, (func(float) = float)::in,
               particle::in, particle::out,
               best_global_position::in, best_global_position::out,
               random.supply::mdi, random.supply::muo) is det.

update(params(InertiaWeight, CognitiveParam, SocialParam), {Lo, Hi}, F,
       particle(X1, V1, P1), particle(X2, V2, P2),
       G1, G2,
       !RandomSupply) :-
    random({0.0, 1.0}, RP, !RandomSupply),
    random({0.0, 1.0}, RG, !RandomSupply),
    V2 = InertiaWeight * V1
       + CognitiveParam * RP * (P1 - X1)
       + SocialParam * RG * (G1 - X1),
    X2 = X1 + V2,
    (   if      Lo =< X2, X2 =< Hi, F(X2) >= F(P1)
        then    P2 = X2,
                G2 = ( if F(X2) >= F(G1) then X2 else G1 )
        else    P2 = P1,
                G2 = G1
    ).

%-----------------------------------------------------------------------------%

run_pso(M, N, Params, Bounds, Direction, F, G, !RandomSupply) :-
    init_pso(M, Bounds, F, Particles, G0, !RandomSupply),
    iterate_pso(N, Params, Bounds, Direction, F,
                Particles, _,
                G0, G,
                !RandomSupply).


:- pred iterate_pso(int::in, params::in, bounds::in, direction::in,
                    (func(float) = float)::in,
                    particles::in, particles::out,
                    best_global_position::in, best_global_position::out,
                    random.supply::mdi, random.supply::muo) is det.

iterate_pso(N, Params, Bounds, Direction, F,
            !Particles, !G, !RandomSupply) :-
    if      N =< 0
    then    true
    else    pso(Params, Bounds, Direction, F, !Particles, !G, !RandomSupply),
            iterate_pso(N-1, Params, Bounds, Direction, F, !Particles, !G,
                        !RandomSupply).

%-----------------------------------------------------------------------------%
:- end_module util.pso.
%-----------------------------------------------------------------------------%

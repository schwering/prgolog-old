%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.cont.m.
% Main author: schwering.
%
% Basic action theory (BAT) for driving with two simple actions, set_yaw and
% set_veloc that control the steering and speed of the vehicle.
%
%-----------------------------------------------------------------------------%

:- module domain.car.cont.

:- interface.

:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module prgolog.nice.
:- use_module random.

:- include_module io_util.

%-----------------------------------------------------------------------------%

:- type prim
    --->    set_veloc(agent, mps, mps, maybe(random.supply),
                      maybe(vargen), list(constraint), time)
    ;       set_yaw(agent, lane, rad, rad, maybe(random.supply), maybe(vargen),
                    list(constraint), time)
    ;       wait_for(ccformula(prim), maybe(vargen), list(constraint), time)
   %;       match(s, ccformula(prim), maybe(vargen), list(constraint), time)
    ;       match(obs, maybe(vargen), list(constraint), time)
    ;       eval(ccformula(prim), maybe(vargen), list(constraint), time)
    ;       init_env(env)
    ;       seed(int).
:- type primf == primf(prim).
:- type proc == proc(prim).
:- type sit == sit(prim).
:- type prog == prog(prim).
:- type conf == conf(prim).

%-----------------------------------------------------------------------------%

:- instance bat(prim).
:- instance obs_bat(prim, obs).
:- instance pr_bat(prim, obs, env).

%-----------------------------------------------------------------------------%

:- func notime = (time::out) is det.

:- func sitlen(sit::in) = (int::out) is det.

%-----------------------------------------------------------------------------%

:- func start(sit::in) = (time::out) is det.
:- func now(sit::in) = (tfunc::out) is det.
:- func random_supply(sit::in) = (random.supply::out) is det.
:- func vargen(sit::in) = (vargen::out) is det.
:- func constraints(sit::in) = (list(constraint)::out) is det.
:- func veloc(agent::in, sit::in) = (mps::out) is det.
:- func yaw(agent::in, sit::in) = (mps::out) is det.
:- func x(agent::in, sit::in) = (tfunc::out) is det.
:- func y(agent::in, sit::in) = (tfunc::out) is det.
:- func x_tol(agent::in, sit::in) = (m::out) is det.
:- func y_tol(agent::in, sit::in) = (m::out) is det.

%-----------------------------------------------------------------------------%

:- func on_right_lane(agent) = ccformula(prim) is det.
:- func on_left_lane(agent) = ccformula(prim) is det.
:- func behind(agent, agent) = ccformula(prim) is det.

%-----------------------------------------------------------------------------%

:- pred poss(prim::in, sit::in) is semidet.

%-----------------------------------------------------------------------------%

:- func lookahead(sit) = lookahead is det.

%-----------------------------------------------------------------------------%

:- func reward(sit) = reward.
:- mode reward(in) = out is det.

%-----------------------------------------------------------------------------%

:- func set_veloc_st(agent, mps) `with_type` primf.
:- func set_yaw_st(agent, lane, rad) `with_type` primf.

%-----------------------------------------------------------------------------%
:- func straight_left(agent) `with_type` proc.
:- func straight_right(agent) `with_type` proc.
:- func left_lane_change(agent) `with_type` proc.
:- func right_lane_change(agent) `with_type` proc.
:- func cruise(agent) `with_type` proc.
:- func overtake(agent, agent) `with_type` proc.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module math.
:- import_module prgolog.nice.

%-----------------------------------------------------------------------------%

:- func r0 = (m::out) is det.
:- func mu = (float::out) is det.
:- func g = (mpss::out) is det.

r0 = 5.0.
mu = 0.8.
g = 9.81.


:- func r(mps::in) = (m::out) is det.
:- func t_pos(mps::in, rad::in) = (m::out) is det.
:- func t_neg(mps::in, rad::in) = (m::out) is det.

r(V) = r0 + V*V / (g*mu).
t_pos(V, A) = A * r(V) / V.
t_neg(V, A) = -1.0 * t_pos(V, A).


:- func r_(mps::in) = (float::out) is det.
:- func t_pos_v(mps::in, rad::in) = (float::out) is det.
:- func t_pos_a(mps::in, rad::in) = (float::out) is det.
:- func t_neg_v(mps::in, rad::in) = (float::out) is det.
:- func t_neg_a(mps::in, rad::in) = (float::out) is det.

r_(V) = 2.0 * V / (g*mu).
t_pos_v(V, A) = A * (r_(V) * V - r(V)) / (V*V).
t_pos_a(V, _A) = r(V) / V.
t_neg_v(V, A) = -1.0 * t_pos_v(V, A).
t_neg_a(V, A) = -1.0 * t_pos_a(V, A).


:- func tang_t_pos(mps::in, rad::in, mps::in, rad::in) = (s::out) is det.
:- func tang_t_neg(mps::in, rad::in, mps::in, rad::in) = (s::out) is det.

tang_t_pos(VV, AA, V, A) = T :-
    Tangential = t_pos(VV, AA),
    SlopeVV = t_pos_v(VV, AA),
    SlopeAA = t_pos_a(VV, AA),
    Constant = Tangential - VV * SlopeVV - AA * SlopeAA,
    T = Constant + V * SlopeVV + A * SlopeAA.

tang_t_neg(VV, AA, V, A) = T :-
    Tangential = t_neg(VV, AA),
    SlopeVV = t_neg_v(VV, AA),
    SlopeAA = t_neg_a(VV, AA),
    Constant = Tangential - VV * SlopeVV - AA * SlopeAA,
    T = Constant + V * SlopeVV + A * SlopeAA.


:- func plane1_pos(mps::in, rad::in) = (s::out) is det.
:- func plane2_pos(mps::in, rad::in) = (s::out) is det.
:- func plane1_neg(mps::in, rad::in) = (s::out) is det.
:- func plane2_neg(mps::in, rad::in) = (s::out) is det.

plane1_pos(V, A) = tang_t_pos(60.0,  pi, V, A).
plane2_pos(V, A) = tang_t_pos( 7.0, 0.0, V, A).
plane1_neg(V, A) = tang_t_neg(60.0, -pi, V, A).
plane2_neg(V, A) = tang_t_neg( 7.0, 0.0, V, A).

%-----------------------------------------------------------------------------%

notime = constant(-1.0).

%-----------------------------------------------------------------------------%

sitlen(s0) = 0.
sitlen(do(_, S)) = 1 + sitlen(S).

%-----------------------------------------------------------------------------%

start(s0) = constant(0.0).
%start(s0) = constant(T) :- initial_time(T).
start(do(A, S)) = T :-
    (   A = set_veloc(_, _, _, _, _, _, T)
    ;   A = set_yaw(_, _, _, _, _, _, _, T)
    ;   A = wait_for(_, _, _, T)
    ;   A = match(_, _, _, T)
    ;   A = eval(_, _, _, T)
    ;   A = init_env(env(T0, _)), T = constant(T0)
    ;   A = seed(_), T = start(S)
    ).

%-----------------------------------------------------------------------------%

now(_) = ( func(T) = T ).

%-----------------------------------------------------------------------------%

random_supply(s0) = RS :-
    random.init(0, RS).
random_supply(do(A, S)) = RS :-
    if      (   A = set_veloc(_, _, _, yes(RS0), _, _, _)
            ;   A = set_yaw(_, _, _, _, yes(RS0), _, _, _)
            ;   A = seed(Seed), random.init(Seed, RS0) )
    then    RS = RS0
    else    RS = random_supply(S).

%-----------------------------------------------------------------------------%

vargen(s0) = init_vargen.
vargen(do(A, S)) = VG :-
    if      (   A = set_veloc(_, _, _, _, yes(VG0), _, _)
            ;   A = set_yaw(_, _, _, _, _, yes(VG0), _, _)
            ;   A = wait_for(_, yes(VG0), _, _)
            ;   A = match(_, yes(VG0), _, _)
            ;   A = eval(_, yes(VG0), _, _)
            )
    then    VG = VG0
    else    VG = vargen(S).

%-----------------------------------------------------------------------------%

constraints(s0) = [].
constraints(do(A, S)) = Cs ++ constraints(S) :-
    (   A = set_veloc(_, _, _, _, _, Cs, _)
    ;   A = set_yaw(_, _, _, _, _, _, Cs, _)
    ;   A = wait_for(_, _, Cs, _)
    ;   A = match(_, _, Cs, _)
    ;   A = eval(_, _, Cs, _)
    ;   A = init_env(_), Cs = []
    ;   A = seed(_), Cs = []
    ).

%-----------------------------------------------------------------------------%

veloc(_, s0) = 0.0.
veloc(Agent, do(A, S)) = Veloc :-
    if      A = init_env(env(_, Map)), info(V0, _, _) = Map^elem(Agent)
    then    Veloc = V0
    else if A = set_veloc(Agent, V0, _, _, _, _, _)
    then    Veloc = V0
    else    Veloc = veloc(Agent, S).

%-----------------------------------------------------------------------------%

yaw(_, s0) = 0.0.
yaw(Agent, do(A, S)) = Rad :-
    if      A = init_env(env(_, Map)), info(_, Rad0, _) = Map^elem(Agent)
    then    Rad = Rad0
    else if A = set_yaw(Agent, _, Rad0, _, _, _, _, _)
    then    Rad = Rad0
    else    Rad = yaw(Agent, S).

%-----------------------------------------------------------------------------%

x(_, s0) = ( func(_) = constant(0.0) ).
%x(Agent, s0) = ( func(_) = constant(X) ) :- initial(Agent, X, _).
x(Agent, do(A, S)) = X :-
    if      A = init_env(env(_, Map)), info(_, _, p(X0, _)) = Map^elem(Agent)
    then    X = ( func(_) = constant(X0) )
    else if (   A = set_veloc(Agent, Veloc, _, _, _, _, T0),
                Rad = yaw(Agent, S)
            ;   A = set_yaw(Agent, _, Rad, _, _, _, _, T0),
                Veloc = veloc(Agent, S)
            )
    then    X = ( func(T) = cos(Rad) * Veloc * (T - T0) + x(Agent, S)(T0) )
    else    X = x(Agent, S).

%-----------------------------------------------------------------------------%

y(_, s0) = ( func(_) = constant(0.0) ).
%y(Agent, s0) = ( func(_) = constant(Y) ) :- initial(Agent, _, Y).
y(Agent, do(A, S)) = Y :-
    if      A = init_env(env(_, Map)), info(_, _, p(_, Y0)) = Map^elem(Agent)
    then    Y = ( func(_) = constant(Y0) )
    else if (   A = set_veloc(Agent, Veloc, _, _, _, _, T0),
                Rad = yaw(Agent, S)
            ;   A = set_yaw(Agent, _, Rad, _, _, _, _, T0),
                Veloc = veloc(Agent, S)
            )
    then    Y = ( func(T) = sin(Rad) * Veloc * (T - T0) + y(Agent, S)(T0) )
    else    Y = y(Agent, S).

%-----------------------------------------------------------------------------%

x_tol(_, s0) = 0.0.
x_tol(Agent, do(A, S)) = Tol :-
    if      A = set_veloc(Agent, _, Tol0, _, _, _, _)
    then    Tol = Tol0
    else    Tol = x_tol(Agent, S).

%-----------------------------------------------------------------------------%

y_tol(_, s0) = 0.0.
y_tol(Agent, do(A, S)) = Tol :-
    if      A = set_yaw(Agent, _, _, Tol0, _, _, _, _)
    then    Tol = Tol0
    else    Tol = y_tol(Agent, S).

%-----------------------------------------------------------------------------%

on_right_lane(Agent) = ( func(T, S) = [
        constant(-4.5) `=<` y(Agent, S)(T),
                            y(Agent, S)(T) `=<` constant(-0.5)
    ] ).

on_left_lane(Agent) = ( func(T, S) = [
        constant(0.5) `=<` y(Agent, S)(T),
                           y(Agent, S)(T) `=<` constant(4.5)
    ] ).

behind(Agent0, Agent1) = ( func(T, S) = [
        x(Agent0, S)(T) `=<` x(Agent1, S)(T)
    ] ).

%-----------------------------------------------------------------------------%

:- func filter_empty_cstrs(list(constraint)::in) = (list(constraint)::out)
    is det.

filter_empty_cstrs(Cs) = negated_filter(holds_trivially, Cs).


:- func set_veloc(agent, mps, mps, maybe(random.supply)) `with_type` primf.

set_veloc(Agent, V, Tol, RS, S) =
  set_veloc(Agent, V, Tol, RS, yes(VG), Cs, T) :-
    T = new_variable(vargen(S), VG),
    Cs = filter_empty_cstrs([T `>=` start(S)]).


:- func set_yaw(agent, lane, rad, rad, maybe(random.supply)) `with_type` primf.

set_yaw(Agent, Lane, Y, Tol, RS, S) =
  set_yaw(Agent, Lane, Y, Tol, RS, yes(VG), Cs, T) :-
    T = new_variable(vargen(S), VG),
    (   Lane = right, OnLane = on_right_lane(Agent)(T, S)
    ;   Lane = left,  OnLane = on_left_lane(Agent)(T, S)
    ),
    Cs = filter_empty_cstrs([T `>=` start(S)] ++ OnLane).


:- func wait_for(ccformula(prim)) `with_type` primf.

wait_for(G, S) = wait_for(G, yes(VG), Cs, T) :-
    T = new_variable(vargen(S), VG),
    Cs = filter_empty_cstrs([T `>=` start(S)] ++ G(T, S)).


:- func match(obs, time) `with_type` primf.

match(Obs, T, S) = match(Obs, yes(VG), Cs, T) :-
    VG = vargen(S),
    {_, OF} = obs2ccformula(Obs),
    Cs = filter_empty_cstrs([T `>=` start(S), T `=` T] ++ OF(T, S)).


:- func eval(ccformula(prim)) `with_type` primf.

eval(G, S) = eval(G, yes(VG), Cs, T) :-
    T = new_variable(vargen(S), VG),
    Cs = filter_empty_cstrs([T `=` start(S)] ++ G(T, S)).

%-----------------------------------------------------------------------------%

poss(set_veloc(_, _, _, _, yes(VG), Cs0, _), S) :-
    Cs1 = Cs0 ++ constraints(S),
    solve(VG, Cs1).

poss(set_yaw(_, _, _, _, _, yes(VG), Cs0, _), S) :-
    Cs1 = Cs0 ++ constraints(S),
    solve(VG, Cs1).

poss(wait_for(_, yes(VG), Cs0, _), S) :-
    Cs1 = Cs0 ++ constraints(S),
    solve(VG, Cs1).

poss(match(_, yes(VG), Cs0, _), S) :-
    Cs1 = Cs0 ++ constraints(S),
    solve(VG, Cs1).

poss(eval(_, yes(VG), Cs0, _), S) :-
    Cs1 = Cs0 ++ constraints(S),
    solve(VG, Cs1).

poss(init_env(_), _).

poss(seed(_), _).

%-----------------------------------------------------------------------------%

lookahead(_S) = 4.

%-----------------------------------------------------------------------------%

reward(s0) = 0.0.
reward(do(A, S)) =
    (   if      A = match(_, _, _, _)
        then    reward(S) + 1.0
        else    reward(S)
    ).

:- func reward(prog, sit) = reward.
:- mode reward(in, in) = out is det.

reward(_, S) = reward(S).

%-----------------------------------------------------------------------------%

:- pred random_normal(float::out, float::out,
                      random.supply::in, random.supply::out) is det.

random_normal(X1, X2, !RS) :-
    random.randmax(Max, !RS),
    random.random(R1, !RS),
    random.random(R2, !RS),
    U1 = float(R1) / float(Max) * 2.0 - 1.0,
    U2 = float(R2) / float(Max) * 2.0 - 1.0,
    Q = U1*U1 + U2*U2,
    (   if      Q > 1.0
        then    random_normal(X1, X2, !RS)
        else    P = sqrt(-2.0 * ln(Q) / Q),
                X1 is U1 * P,
                X2 is U1 * P
    ).


:- pred random_lognormal(float::in, float::in,
                         float::out, float::out,
                         random.supply::in, random.supply::out) is det.

random_lognormal(Mu, Sigma, Y1, Y2, !RS) :-
    random_normal(X1, X2, !RS),
    Y1 = pow(e, Mu + Sigma * X1),
    Y2 = pow(e, Mu + Sigma * X2).


set_veloc_st(Agent, V, S) =
  set_veloc(Agent, V, Tol, yes(RS1), S) :-
    RS0 = random_supply(S),
    random_lognormal(1.0, 1.0, Tol, _, RS0, RS1).

set_yaw_st(Agent, Lane, Yaw, S) =
  set_yaw(Agent, Lane, Yaw, Tol, yes(RS1), S) :-
    (   if      Yaw = 0.0
        then    Mu = -0.7, Sigma = 0.7, TolMax = 2.0
        else    Mu = -0.2, Sigma = 1.0, TolMax = 2.5
    ),
    RS0 = random_supply(S),
    random_lognormal(Mu, Sigma, _, Tol0, RS0, RS1),
    Tol = 0.33 + min(Tol0, TolMax).

%-----------------------------------------------------------------------------%

straight_left(Agent) = P :-
    P = atomic(
            b(set_yaw_st(Agent, left, deg2rad(0.0))) `;`
            nil% a(eval(on_left_lane(Agent)))
        ).

straight_right(Agent) = P :-
    P = atomic(
            b(set_yaw_st(Agent, right, deg2rad(0.0))) `;`
            nil% b(eval(on_right_lane(Agent)))
        ).

left_lane_change(Agent) = P :-
    P = atomic(
            ( b(set_yaw_st(Agent, right, deg2rad(14.0))) or
              b(set_yaw_st(Agent, right, deg2rad(12.0))) or
              b(set_yaw_st(Agent, right, deg2rad(10.0))) or
              b(set_yaw_st(Agent, right, deg2rad(8.0))) or
              b(set_yaw_st(Agent, right, deg2rad(6.0))) ) %`;`
            %b(eval(on_right_lane(Agent)))
        ) `;`
        p(((func) = straight_left(Agent))).

right_lane_change(Agent) = P :-
    P = atomic(
            ( b(set_yaw_st(Agent, left, deg2rad(-14.0))) or
              b(set_yaw_st(Agent, left, deg2rad(-12.0))) or
              b(set_yaw_st(Agent, left, deg2rad(-10.0))) or
              b(set_yaw_st(Agent, left, deg2rad(-8.0))) or
              b(set_yaw_st(Agent, left, deg2rad(-6.0))) ) %`;`
            %b(eval(on_right_lane(Agent)))
        ) `;`
        p(((func) = straight_right(Agent))).

cruise(Agent) = P :-
    P = p(((func) = straight_right(Agent))) `;`
        b(set_veloc_st(Agent, 15.09)).

overtake(Agent, Victim) = P :-
    P = b(set_veloc_st(Agent, 20.8)) `;` % Why do we need this stupid action?
                                         % Without it, plan recognition fails.
        b(set_yaw_st(Agent, right, 0.0)) `;`
        b(eval(on_right_lane(Agent) and on_right_lane(Victim) and Agent `behind` Victim)) `;`
        p(((func) = straight_right(Agent))) `;`
        ((
            p(((func) = left_lane_change(Agent))) `;`
            b(wait_for(Victim `behind` Agent)) `;`
            p(((func) = right_lane_change(Agent)))
        ) // (
            b(set_veloc_st(Agent, 20.8))
        )) `;`
        b(eval(on_right_lane(Agent) and Victim `behind` Agent)).

%-----------------------------------------------------------------------------%

:- pred is_obs_action(prim::in) is semidet.

is_obs_action(match(_, _, _, _)).


:- pred is_obs_prog(pseudo_atom(prim)::in) is semidet.

is_obs_prog(atom(primf(AF))) :- cont.is_obs_action(AF(s0)).


:- func last_match(sit(prim)) = prim is semidet.

last_match(do(A, S)) = ( if cont.is_obs_action(A) then A else last_match(S) ).


:- pred covered_by_match(sit(prim)::in) is semidet.

covered_by_match(S) :-
    match(_, _, _, T0) = last_match(S),
    C = (start(S) `=` T0),
    solve(vargen(S), [C] ++ constraints(S)).


:- func obs2ccformula(obs::in) = ({s, ccformula(prim)}::out) is det.

obs2ccformula(obs(OT, AgentPositions)) = {OT, OF} :-
    OF = ( func(T, S) =
        foldr(( func((Agent - info(_, _, Pos)), Cs) = [C1, C2, C3, C4] ++ Cs :-
            C1 = constant(x(Pos) - x_tol(Agent, S)) `=<` x(Agent, S)(T),
            C2 = constant(x(Pos) + x_tol(Agent, S)) `>=` x(Agent, S)(T),
            C3 = constant(y(Pos) - y_tol(Agent, S)) `=<` y(Agent, S)(T),
            C4 = constant(y(Pos) + y_tol(Agent, S)) `>=` y(Agent, S)(T)
        ), AgentPositions, [])
    ).


:- func obs2match(obs) = pseudo_atom(prim).

obs2match(Obs) = atom(primf(match(Obs, constant(OT)))) :-
    {OT, _} = obs2ccformula(Obs).

%-----------------------------------------------------------------------------%

:- instance bat(prim) where [
    pred(poss/2) is cont.poss,
    func(reward/2) is cont.reward,
    func(lookahead/1) is cont.lookahead
].

:- instance obs_bat(prim, obs) where [
    pred(is_obs_action/1) is cont.is_obs_action,
    pred(is_obs_prog/1) is cont.is_obs_prog,
    pred(covered_by_obs/1) is cont.covered_by_match,
    func(obs_to_action/1) is cont.obs2match
].

:- instance pr_bat(prim, obs, env) where [
    seed_init_sit(I) = do(seed(I), s0),
    init_env_sit(env(T, Map), S) = do(init_env(env(T, Map)), S)
].

%-----------------------------------------------------------------------------%
:- end_module domain.car.cont.
%-----------------------------------------------------------------------------%

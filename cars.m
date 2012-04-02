%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: cars.m.
% Main author: schwering.
%
% Basic action theory (BAT) for driving with two simple actions, set_yaw and
% set_veloc that control the steering and speed of the vehicle.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module cars.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module prgolog.fluent.
:- import_module prgolog.nice.
:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module float.
:- import_module gc.
:- import_module list.
:- import_module math.
:- import_module map.
:- import_module maybe.
:- use_module random.
:- import_module string.
:- import_module solutions.
:- import_module times.
:- import_module thread.
:- import_module thread.channel.
:- import_module require.
:- import_module thread.mvar.
:- import_module thread.semaphore.
:- import_module table_statistics.

%-----------------------------------------------------------------------------%

:- type degree == float.
:- type rad == float.
:- type kmph == float.
:- type mps == float.
:- type mpss == float.
:- type m == float.
:- type s == float.

:- type agent ---> a ; b.
:- type lane ---> left ; right.
:- type agent_info == {mps, rad, m, m}.
:- type prim --->
        set_veloc(agent, mps, mps, maybe(random.supply),
                  maybe(vargen), list(constraint), time)
    ;   set_yaw(agent, lane, rad, rad, maybe(random.supply), maybe(vargen),
                list(constraint), time)
    ;   wait_for(ccformula(prim), maybe(vargen), list(constraint), time)
    %;   match(s, ccformula(prim), maybe(vargen), list(constraint), time)
    ;   match(s, obs, maybe(vargen), list(constraint), time)
    ;   eval(ccformula(prim), maybe(vargen), list(constraint), time)
    ;   init_env(s, assoc_list(agent, agent_info))
    ;   seed(int).
:- type stoch --->  set_veloc_st(agent, mps)
                ;   set_yaw_st(agent, lane, rad).
:- type procedure --->  straight_left(agent)
                    ;   straight_right(agent)
                    ;   left_lane_change(agent)
                    ;   right_lane_change(agent)
                    ;   cruise(agent)
                    ;   overtake(agent, agent).

%-----------------------------------------------------------------------------%

:- func deg2rad(degree::in) = (rad::out) is det.
deg2rad(Deg) = Deg / 180.0 * pi.

:- func rad2deg(rad::in) = (degree::out) is det.
rad2deg(Rad) = Rad * 180.0 / pi.

:- func kmh2ms(kmph::in) = (mps::out) is det.
kmh2ms(Kmh) = Kmh / 3.6.

:- func ms2kmh(kmph::in) = (mps::out) is det.
ms2kmh(Ms) = Ms * 3.6.


:- func deg_zero = (degree::out) is det.
:- func deg_min = (degree::out) is det.
:- func deg_max = (degree::out) is det.

deg_zero = 0.0.
deg_min = deg_zero - 25.0.
deg_max = deg_zero - 25.0.

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

:- func notime = (time::out) is det.

notime = constant(-1.0).

%-----------------------------------------------------------------------------%

:- func sitlen(sit(prim)::in) = (int::out) is det.

sitlen(s0) = 0.
sitlen(do(_, S)) = 1 + sitlen(S).

%-----------------------------------------------------------------------------%

:- func start(sit(prim)::in) = (time::out) is det.

%start(s0) = constant(0.0).
start(s0) = constant(T) :- initial_time(T).
start(do(A, S)) = T :-
    (   A = set_veloc(_, _, _, _, _, _, T)
    ;   A = set_yaw(_, _, _, _, _, _, _, T)
    ;   A = wait_for(_, _, _, T)
    ;   A = match(_, _, _, _, T)
    ;   A = eval(_, _, _, T)
    ;   A = init_env(T0, _), T = constant(T0)
    ;   A = seed(_), T = start(S)
    ).

%-----------------------------------------------------------------------------%

:- func now(sit(prim)::in) = (tfunc::out).

now(_) = ( func(T) = T ).

%-----------------------------------------------------------------------------%

:- func random_supply(sit(prim)::in) = (random.supply::out) is det.

random_supply(s0) = RS :-
    random.init(0, RS).
random_supply(do(A, S)) = RS :-
    if      (   A = set_veloc(_, _, _, yes(RS0), _, _, _)
            ;   A = set_yaw(_, _, _, _, yes(RS0), _, _, _)
            ;   A = seed(Seed), random.init(Seed, RS0) )
    then    RS = RS0
    else    RS = random_supply(S).

%-----------------------------------------------------------------------------%

:- func vargen(sit(prim)::in) = (vargen::out) is det.

vargen(s0) = init_vargen.
vargen(do(A, S)) = VG :-
    if      (   A = set_veloc(_, _, _, _, yes(VG0), _, _)
            ;   A = set_yaw(_, _, _, _, _, yes(VG0), _, _)
            ;   A = wait_for(_, yes(VG0), _, _)
            ;   A = match(_, _, yes(VG0), _, _)
            ;   A = eval(_, yes(VG0), _, _)
            )
    then    VG = VG0
    else    VG = vargen(S).

%-----------------------------------------------------------------------------%

:- func constraints(sit(prim)::in) = (list(constraint)::out) is det.

constraints(s0) = [].
constraints(do(A, S)) = Cs ++ constraints(S) :-
    (   A = set_veloc(_, _, _, _, _, Cs, _)
    ;   A = set_yaw(_, _, _, _, _, _, Cs, _)
    ;   A = wait_for(_, _, Cs, _)
    ;   A = match(_, _, _, Cs, _)
    ;   A = eval(_, _, Cs, _)
    ;   A = init_env(_, _), Cs = []
    ;   A = seed(_), Cs = []
    ).

%-----------------------------------------------------------------------------%

:- func veloc(agent::in, sit(prim)::in) = (mps::out) is det.

veloc(_, s0) = 0.0.
veloc(Agent, do(A, S)) = Veloc :-
    if      A = init_env(_, Map)
    then    {Veloc, _, _, _} = Map^det_elem(Agent)
    else if A = set_veloc(Agent, V0, _, _, _, _, _)
    then    Veloc = V0
    else    Veloc = veloc(Agent, S).

%-----------------------------------------------------------------------------%

:- func yaw(agent::in, sit(prim)::in) = (mps::out) is det.

yaw(_, s0) = 0.0.
yaw(Agent, do(A, S)) = Rad :-
    if      A = init_env(_, Map)
    then    {_, Rad, _, _} = Map^det_elem(Agent)
    else if A = set_yaw(Agent, _, Rad0, _, _, _, _, _)
    then    Rad = Rad0
    else    Rad = yaw(Agent, S).

%-----------------------------------------------------------------------------%

:- func x(agent::in, sit(prim)::in) = (tfunc::out) is det.

%x(_, s0) = ( func(_) = constant(0.0) ).
x(Agent, s0) = ( func(_) = constant(X) ) :-
    initial(Agent, X, _).
x(Agent, do(A, S)) = X :-
    if      A = init_env(_, Map)
    then    {_, _, X0, _} = Map^det_elem(Agent),
            X = ( func(_) = constant(X0) )
    else if (   A = set_veloc(Agent, Veloc, _, _, _, _, T0), Rad = yaw(Agent, S)
            ;   A = set_yaw(Agent, _, Rad, _, _, _, _, T0), Veloc = veloc(Agent, S)
            )
    then    X = ( func(T) = cos(Rad) * Veloc * (T - T0) +
                            x(Agent, S)(T0) )
    else    X = x(Agent, S).

%-----------------------------------------------------------------------------%

:- func y(agent::in, sit(prim)::in) = (tfunc::out) is det.

%y(_, s0) = ( func(_) = constant(0.0) ).
y(Agent, s0) = ( func(_) = constant(Y) ) :-
    initial(Agent, _, Y).
y(Agent, do(A, S)) = Y :-
    if      A = init_env(_, Map)
    then    {_, _, _, Y0} = Map^det_elem(Agent),
            Y = ( func(_) = constant(Y0) )
    else if (   A = set_veloc(Agent, Veloc, _, _, _, _, T0), Rad = yaw(Agent, S)
            ;   A = set_yaw(Agent, _, Rad, _, _, _, _, T0), Veloc = veloc(Agent, S)
            )
    then    Y = ( func(T) = sin(Rad) * Veloc * (T - T0) +
                            y(Agent, S)(T0) )
    else    Y = y(Agent, S).

%-----------------------------------------------------------------------------%

:- func x_tol(agent::in, sit(prim)::in) = (m::out) is det.

x_tol(_, s0) = 0.0.
x_tol(Agent, do(A, S)) = Tol :-
    if      A = set_veloc(Agent, _, Tol0, _, _, _, _)
    then    Tol = Tol0
    else    Tol = x_tol(Agent, S).


%-----------------------------------------------------------------------------%

:- func y_tol(agent::in, sit(prim)::in) = (m::out) is det.

y_tol(_, s0) = 0.0.
y_tol(Agent, do(A, S)) = Tol :-
    if      A = set_yaw(Agent, _, _, Tol0, _, _, _, _)
    then    Tol = Tol0
    else    Tol = y_tol(Agent, S).

%-----------------------------------------------------------------------------%

:- func on_right_lane(agent) = ccformula(prim) is det.

on_right_lane(Agent) = ( func(T, S) = [
        constant(-4.5) `=<` y(Agent, S)(T),
                            y(Agent, S)(T) `=<` constant(-0.5)
    ] ).

:- func on_left_lane(agent) = ccformula(prim) is det.

on_left_lane(Agent) = ( func(T, S) = [
        constant(0.5) `=<` y(Agent, S)(T),
                           y(Agent, S)(T) `=<` constant(4.5)
    ] ).

:- func behind(agent, agent) = ccformula(prim) is det.

behind(Agent0, Agent1) = ( func(T, S) = [
        x(Agent0, S)(T) `=<` x(Agent1, S)(T)
    ] ).

%-----------------------------------------------------------------------------%

:- func filter_empty_cstrs(list(constraint)::in) = (list(constraint)::out) is det.

filter_empty_cstrs(Cs) = negated_filter(holds_trivially, Cs).


:- pred poss(prim::in, prim::out, sit(prim)::in) is semidet.

poss(set_veloc(Agent, V, Tol, RS, no, [], notime),
     set_veloc(Agent, V, Tol, RS, yes(VG), Cs0, T),
     S) :-
    T = new_variable(vargen(S), VG),
    Cs0 = filter_empty_cstrs([T `>=` start(S)]),
    Cs1 = Cs0 ++ constraints(S),
    solve(VG, Cs1).

poss(set_yaw(Agent, Lane, Y, Tol, RS, no, [], notime),
     set_yaw(Agent, Lane, Y, Tol, RS, yes(VG), Cs0, T),
     S) :-
    T = new_variable(vargen(S), VG),
    (   Lane = right, OnLane = on_right_lane(Agent)(T, S)
    ;   Lane = left,  OnLane = on_left_lane(Agent)(T, S)
    ),
    Cs0 = filter_empty_cstrs([T `>=` start(S)] ++ OnLane),
    Cs1 = Cs0 ++ constraints(S),
    solve(VG, Cs1).

poss(wait_for(G, no, [], notime),
     wait_for(G, yes(VG), Cs0, T),
     S) :-
    T = new_variable(vargen(S), VG),
    Cs0 = filter_empty_cstrs([T `>=` start(S)] ++ G(T, S)),
    Cs1 = Cs0 ++ constraints(S),
    solve(VG, Cs1).

poss(match(OT, Obs, no, [], notime),
     match(OT, Obs, yes(VG), Cs0, T),
     S) :-
    T = new_variable(vargen(S), VG),
    {_, OF} = obs2ccformula(Obs),
    Cs0 = filter_empty_cstrs([T `>=` start(S), T `=` constant(OT)] ++ OF(T, S)),
    Cs1 = Cs0 ++ constraints(S),
    solve(VG, Cs1).

poss(eval(G, no, [], notime),
     eval(G, yes(VG), Cs0, T),
     S) :-
    T = new_variable(vargen(S), VG),
    Cs0 = filter_empty_cstrs([T `=` start(S)] ++ G(T, S)),
    Cs1 = Cs0 ++ constraints(S),
    solve(VG, Cs1).

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


:- pred random_outcome(stoch::in, prim::out, sit(prim)::in) is det.

random_outcome(set_veloc_st(Agent, V),
               set_veloc(Agent, V, Tol, yes(RS1), no, [], notime),
               S) :-
    RS0 = random_supply(S),
    random_lognormal(1.0, 1.0, Tol, _, RS0, RS1).

random_outcome(set_yaw_st(Agent, Lane, Yaw),
               set_yaw(Agent, Lane, Yaw, Tol, yes(RS1), no, [], notime),
               S) :-
    (   if      Yaw = 0.0
        then    Mu = -0.7, Sigma = 0.7, TolMax = 2.0
        else    Mu = -0.2, Sigma = 1.0, TolMax = 2.5
    ),
    %Tol = min(0.5 + abs(Yaw) * 4.0, 2.5),
    RS0 = random_supply(S),
    random_lognormal(Mu, Sigma, _, Tol0, RS0, RS1),
    Tol = min(Tol0, TolMax).

%-----------------------------------------------------------------------------%

:- func lookahead(sit(prim)) = lookahead is det.

lookahead(_S) = 3.

%-----------------------------------------------------------------------------%

:- func new_lookahead(lookahead, atom(prim, stoch)) = lookahead is det.

new_lookahead(H, _C) = H - 1.

%-----------------------------------------------------------------------------%

:- func reward(prog(prim, stoch, procedure), sit(prim)) = reward.
:- mode reward(in, in) = out is det.

reward(_, S) = reward(S).

:- func reward(sit(prim)) = reward.
:- mode reward(in) = out is det.

reward(s0) = 0.0.
reward(do(A, S)) =
    (   if      A = match(_, _, _, _, _)
        then    reward(S) + 1.0
        else    reward(S)
    ).

%-----------------------------------------------------------------------------%

:- func match_count(prog(prim, stoch, procedure)) = int.

match_count(seq(P1, P2)) = match_count(P1) + match_count(P2).
match_count(non_det(P1, P2)) = min(match_count(P1), match_count(P2)).
match_count(conc(P1, P2)) = match_count(P1) + match_count(P2).
match_count(star(_)) = 0.
match_count(proc(_)) = 0.
match_count(nil) = 0.
match_count(pseudo_atom(complex(P))) = match_count(P).
match_count(pseudo_atom(atom(A))) =
    ( if A = prim(match(_, _, _, _, _)) then 1 else 0 ).

%-----------------------------------------------------------------------------%

:- pred proc(procedure, prog(prim, stoch, procedure)).
:- mode proc(in, out) is det.

proc(straight_left(Agent), P) :-
    P = atomic(
            b(set_yaw_st(Agent, left, deg2rad(0.0))) `;`
            nil% a(eval(on_left_lane(Agent), no, [], notime))
        ).

proc(straight_right(Agent), P) :-
    P = atomic(
            b(set_yaw_st(Agent, right, deg2rad(0.0))) `;`
            nil% a(eval(on_right_lane(Agent), no, [], notime))
        ).

proc(left_lane_change(Agent), P) :-
    P = atomic(
            ( b(set_yaw_st(Agent, right, deg2rad(14.0))) or
              b(set_yaw_st(Agent, right, deg2rad(12.0))) or
              b(set_yaw_st(Agent, right, deg2rad(10.0))) or
              b(set_yaw_st(Agent, right, deg2rad(8.0))) or
              b(set_yaw_st(Agent, right, deg2rad(6.0))) ) %`;`
            %a(eval(on_right_lane(Agent), no, [], notime))
        ) `;`
        p(straight_left(Agent)).

proc(right_lane_change(Agent), P) :-
    P = atomic(
            ( b(set_yaw_st(Agent, left, deg2rad(-14.0))) or
              b(set_yaw_st(Agent, left, deg2rad(-12.0))) or
              b(set_yaw_st(Agent, left, deg2rad(-10.0))) or
              b(set_yaw_st(Agent, left, deg2rad(-8.0))) or
              b(set_yaw_st(Agent, left, deg2rad(-6.0))) ) %`;`
            %a(eval(on_right_lane(Agent), no, [], notime))
        ) `;`
        p(straight_right(Agent)).

proc(cruise(Agent), P) :-
    P = p(straight_right(Agent)) `;`
        b(set_veloc_st(Agent, 15.09)).

proc(overtake(Agent, Victim), P) :-
    P = a(eval(on_right_lane(Agent)
           and on_right_lane(Victim)
           and Agent `behind` Victim
        , no, [], notime)) `;`
        p(straight_right(Agent)) `;`
        ((
            p(left_lane_change(Agent)) `;`
            a(wait_for(Victim `behind` Agent, no, [], notime)) `;`
            p(right_lane_change(Agent))
        ) // (
            b(set_veloc_st(Agent, 20.8))
        )) `;`
        a(eval(on_right_lane(Agent)
           and Victim `behind` Agent
        , no, [], notime)).

%-----------------------------------------------------------------------------%

:- instance bat(cars.prim, cars.stoch, cars.procedure) where [
    pred(poss/3) is cars.poss,
    pred(random_outcome/3) is cars.random_outcome,
    func(reward/2) is cars.reward,
    func(lookahead/1) is cars.lookahead,
    func(new_lookahead/2) is cars.new_lookahead,
    pred(proc/2) is cars.proc
].

%-----------------------------------------------------------------------------%

:- type obs == {s, agent, m, m, agent, m, m}.
:- type obs_msg ---> init_msg(assoc_list(agent, {mps, rad, m, m}), s) ; obs_msg(obs) ; end_of_obs.

:- pred initial_time(s::out) is det.

initial_time(5.594000).


:- pred initial(agent::in, m::out, m::out) is det.

initial(a, 34.776825, -2.999933).
initial(b, 6.304431, -3.273941).


:- pred obs(s, agent, m, m, agent, m, m).
:- mode obs(out, out, out, out, out, out, out) is multi.
:- mode obs(in, out, out, out, out, out, out) is semidet.

obs(5.594000,  a, 34.776825, -2.999933,   b, 6.304431, -3.273941).
obs(6.100000,  a, 42.410252, -2.999933,   b, 16.886641, -3.226734).
obs(6.606000,  a, 50.046276, -2.999933,   b, 27.465494, -3.191746).
obs(7.112000,  a, 57.682270, -2.999933,   b, 38.043564, -3.213415).
obs(7.618000,  a, 65.313354, -2.999933,   b, 48.628551, -3.218992).
obs(8.132000,  a, 73.062225, -2.999933,   b, 59.107368, -1.065044).
obs(8.632000,  a, 80.600098, -2.999933,   b, 69.129898, 1.880987).
obs(9.132000,  a, 88.137970, -2.999933,   b, 79.493713, 2.901567).
obs(9.632000,  a, 95.675766, -2.999933,   b, 89.940277, 2.991667).
obs(10.132000, a, 103.213608, -2.999933,  b, 100.390549, 3.025797).
obs(10.632000,  a, 110.751450, -2.999933, b, 110.836411, 3.002779).
obs(11.132000,  a, 118.289291, -2.999933, b, 121.284592, 2.924112).
obs(11.632000,  a, 125.827133, -2.999933, b, 131.733932, 2.790201).
obs(12.132000,  a, 133.364975, -2.999933, b, 142.175156, 2.600296).
obs(12.632000,  a, 140.902817, -2.999933, b, 152.542404, 1.616369).
obs(13.132000, a, 148.440659, -2.999933, b, 162.487106, -1.544439).
obs(13.632000, a, 155.988510, -2.999933, b, 172.840927, -2.558414).
obs(14.132000, a, 163.527390, -2.999933, b, 183.293320, -2.678141).
obs(14.632000, a, 171.065231, -2.999933, b, 193.741440, -2.689064).
obs(15.132000, a, 178.591293, -2.999933, b, 204.195236, -2.745777).
obs(15.632000, a, 186.113022, -2.999933, b, 214.648361, -2.857815).
/*
*/


:- pred obs(obs::out) is multi.

obs({T, A0, X0, Y0, A1, X1, Y1}) :- obs(T, A0, X0, Y0, A1, X1, Y1).


:- pred match_action(prim::out) is multi.

match_action(obs2match(Obs)) :- obs(Obs).


:- func obs2ccformula(obs::in) = ({s, ccformula(prim)}::out) is det.

obs2ccformula({OT, A0, X0, Y0, A1, X1, Y1}) = {OT, OF} :-
    C = ( func(F) = constant(F) ),
    OF = ( func(T, S) = [
        C(X0 - x_tol(A0, S)) `=<` x(A0, S)(T),
                                  x(A0, S)(T) `=<` C(X0 + x_tol(A0, S)),
        C(Y0 - y_tol(A0, S)) `=<` y(A0, S)(T),
                                  y(A0, S)(T) `=<` C(Y0 + y_tol(A0, S)),
        C(X1 - x_tol(A1, S)) `=<` x(A1, S)(T),
                                  x(A1, S)(T) `=<` C(X1 + x_tol(A1, S)),
        C(Y1 - y_tol(A1, S)) `=<` y(A1, S)(T),
                                  y(A1, S)(T) `=<` C(Y1 + y_tol(A1, S))
    ] ).


:- func obs2match(obs::in) = (prim::out) is det.

obs2match(Obs) = match(OT, Obs, no, [], notime) :-
    {OT, _} = obs2ccformula(Obs).


:- func append_match(prog(prim, stoch, procedure), prim) =
    prog(prim, stoch, procedure) is det.

append_match(P, O) =
    (   if      P1 = append_match_to_most_right(P, O)
        then    P1
        else    conc(P, pseudo_atom(atom(prim(O))))
    ).


:- func append_match_to_most_right(prog(prim, stoch, procedure), prim) =
                                   prog(prim, stoch, procedure) is semidet.

append_match_to_most_right(seq(P1, P2), M) =
    (   if      Q2 = append_match_to_most_right(P2, M)
        then    seq(P1, Q2)
        else    append_match_to_most_right(P1, M) ).
append_match_to_most_right(non_det(P1, P2), M) =
    (   if      Q2 = append_match_to_most_right(P2, M)
        then    non_det(P1, Q2)
        else    append_match_to_most_right(P1, M) ).
append_match_to_most_right(conc(P1, P2), M) =
    (   if      Q2 = append_match_to_most_right(P2, M)
        then    conc(P1, Q2)
        else    append_match_to_most_right(P1, M) ).
append_match_to_most_right(star(P), M) =
    append_match_to_most_right(P, M).
append_match_to_most_right(M0, M) = seq(M0, pseudo_atom(atom(prim(M)))) :-
    M0 = pseudo_atom(atom(prim(match(_, _, _, _, _)))).


:- func remove_match_sequence(prog(prim, stoch, procedure)) =
    prog(prim, stoch, procedure) is semidet.

remove_match_sequence(conc(P1, P2)) = Q :-
    if          only_match_actions(P2)
    then        Q = P1
    else if     only_match_actions(P1)
    then        Q = P2
    else        false.


:- pred only_match_actions(prog(prim, stoch, procedure)::in) is semidet.

only_match_actions(seq(P1, P2)) :-
    only_match_actions(P1),
    only_match_actions(P2).
only_match_actions(non_det(P1, P2)) :-
    only_match_actions(P1),
    only_match_actions(P2).
only_match_actions(conc(P1, P2)) :-
    only_match_actions(P1),
    only_match_actions(P2).
only_match_actions(star(P)) :-
    only_match_actions(P).
only_match_actions(pseudo_atom(atom(prim(match(_, _, _, _, _))))).
only_match_actions(nil).


:- func last_match(sit(prim)) = prim is semidet.

last_match(do(A, S)) =
    ( if A = match(_, _, _, _, _) then A else last_match(S) ).


:- pred last_action_covered_by_match(sit(prim)::in) is semidet.

last_action_covered_by_match(S) :-
    match(T0, _, _, _, _) = last_match(S),
    C = (start(S) `=` constant(T0)),
    solve(vargen(S), [C] ++ constraints(S)).


:- func append_obs(prog(prim, stoch, procedure), obs) =
    prog(prim, stoch, procedure) is det.

append_obs(P, O) = append_match(P, obs2match(O)).


:- func match_prog = prog(prim, stoch, procedure) is det.

match_prog = Prog :-
    solutions(match_action, Actions),
    Progs = list.map(( func(A) = pseudo_atom(atom(prim(A))) ), Actions),
    Prog = list.foldr(( func(P1, P2) = seq(P1, P2) ), Progs, nil).

%-----------------------------------------------------------------------------%

/*
  XXX this is the old, IO-dependent version of reading obervations from stdin.

      the new version uses foreign code with promise_pure

:- pred read_w(input_stream::in, maybe(string)::out, io::di, io::uo) is det.

read_w(Stream, Word, !IO) :-
    read_word(Stream, RWord, !IO),
    (   RWord = ok(List), Word = yes(from_char_list(List))
    ;   RWord = eof, Word = no
    ;   RWord = error(_), error("IO error")
    ).


:- pred read_agent(input_stream::in, maybe(agent)::out, io::di, io::uo) is det.

read_agent(Stream, Agent, !IO) :-
    read_w(Stream, MaybeWord, !IO),
    (   MaybeWord = yes(Word), Agent = yes(string_to_agent(Word))
    ;   MaybeWord = no, Agent = no
    ).


:- pred read_float(input_stream::in, maybe(float)::out, io::di, io::uo) is det.

read_float(Stream, Float, !IO) :-
    read_w(Stream, MaybeWord, !IO),
    (   MaybeWord = yes(Word), Float = yes(det_to_float(Word))
    ;   MaybeWord = no, Float = no
    ).


:- pred input_obs_generator(input_stream::in, obs_msg::out,
                            io::di, io::uo) is det.

input_obs_generator(Stream, Obs, !IO) :-
    read_w(Stream, MaybeKind, !IO),
    read_float(Stream, MaybeTime, !IO),
    read_agent(Stream, MaybeAgent0, !IO),
    read_float(Stream, MaybeVeloc0, !IO),
    read_float(Stream, MaybeYaw0, !IO),
    read_float(Stream, MaybeX0, !IO),
    read_float(Stream, MaybeY0, !IO),
    read_agent(Stream, MaybeAgent1, !IO),
    read_float(Stream, MaybeVeloc1, !IO),
    read_float(Stream, MaybeYaw1, !IO),
    read_float(Stream, MaybeX1, !IO),
    read_float(Stream, MaybeY1, !IO),
    (   if      MaybeKind = yes("I"),
                MaybeTime = yes(Time),
                MaybeAgent0 = yes(Agent0),
                MaybeVeloc0 = yes(Veloc0),
                MaybeYaw0 = yes(Yaw0),
                MaybeX0 = yes(X0),
                MaybeY0 = yes(Y0),
                MaybeAgent1 = yes(Agent1),
                MaybeVeloc1 = yes(Veloc1),
                MaybeYaw1 = yes(Yaw1),
                MaybeX1 = yes(X1),
                MaybeY1 = yes(Y1)
        then    Map = [(Agent0 - {Veloc0, Yaw0, X0, Y0}),
                       (Agent1 - {Veloc1, Yaw1, X1, Y1})],
                Obs = init_msg(Map, Time)
       else if  MaybeKind = yes("O"),
                MaybeTime = yes(Time),
                MaybeAgent0 = yes(Agent0),
                MaybeX0 = yes(X0),
                MaybeY0 = yes(Y0),
                MaybeAgent1 = yes(Agent1),
                MaybeX1 = yes(X1),
                MaybeY1 = yes(Y1)
        then    Obs = obs_msg({Time, Agent0, X0, Y0, Agent1, X1, Y1})
        else    Obs = end_of_obs
    ).
*/


:- pred input_init_obs(int::uo) is det.

input_init_obs(0).


:- pred input_next_obs(obs_msg::out, int::di, int::uo) is det.

input_next_obs(ObsMsg, I0, I1) :-
    input_next_obs_pure(I0, I1, Ok, Time, AgentS0, Veloc0, Rad0, X0, Y0,
                                          AgentS1, Veloc1, Rad1, X1, Y1),
    (
        Ok = yes,
        % XXX TODO adapt to handle multiple drivers or so
        Agent0 = string_to_agent(AgentS0),
        Agent1 = string_to_agent(AgentS1),
        (   if      I1 = 1
            then    ObsMsg = init_msg([Agent0 - {Veloc0, Rad0, X0, Y0},
                                       Agent1 - {Veloc1, Rad1, X1, Y1}], Time)
            else    ObsMsg = obs_msg({Time, Agent0, X0, Y0, Agent1, X1, Y1})
        )
    ;
        Ok = no,
        ObsMsg = end_of_obs
    ).


:- pragma foreign_decl("C", "
    #define NRECORDS 500
    #define AGENTLEN 15
    struct record {
        double t;
        char agent0[AGENTLEN+1];
        double veloc0;
        double rad0;
        double x0;
        double y0;
        char agent1[AGENTLEN+1];
        double veloc1;
        double rad1;
        double x1;
        double y1;
    };
").


:- pragma foreign_code("C", "
    volatile int max_valid_record = -1;
    struct record records[NRECORDS];
    pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
").


:- pred input_next_obs_pure(int::di, int::uo, bool::out, float::out,
    string::out, float::out, float::out, float::out, float::out,
    string::out, float::out, float::out, float::out, float::out) is det.

:- pragma foreign_proc("C",
    input_next_obs_pure(I0::di, I1::uo, Ok::out, T::out,
        Agent0::out, Veloc0::out, Rad0::out, X0::out, Y0::out,
        Agent1::out, Veloc1::out, Rad1::out, X1::out, Y1::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ok = MR_YES;
    while (I0 > max_valid_record) {
        if (pthread_mutex_lock(&mutex) == 0) {
            /* We don't need this condition, but the next observation may be
             * read later.  So we re-test the loop condition and only read
             * (which may block again) only if reading is really necessary. */
            if (I0 > max_valid_record) {
                struct record r;
                int i = scanf(
                        ""%*c %lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\\n"",
                        &r.t,
                        r.agent0, &r.veloc0, &r.rad0, &r.x0, &r.y0,
                        r.agent1, &r.veloc1, &r.rad1, &r.x1, &r.y1);
                if (i == EOF) {
                    Ok = MR_NO;
                    pthread_mutex_unlock(&mutex);
                    break;
                } else if (i == 11) {
                    Ok = MR_YES;
                    memcpy(&records[max_valid_record+1], &r,
                        sizeof(struct record));
                    ++max_valid_record;
                }
            }
            pthread_mutex_unlock(&mutex);
        }
    }
    if (Ok == MR_YES) {
        T = (MR_Float) records[I0].t;
        Agent0 = MR_make_string_const(records[I0].agent0);
        Veloc0 = (MR_Float) records[I0].veloc0;
        Rad0 = (MR_Float) records[I0].rad0;
        X0 = (MR_Float) records[I0].x0;
        Y0 = (MR_Float) records[I0].y0;
        Agent1 = MR_make_string_const(records[I0].agent1);
        Veloc1 = (MR_Float) records[I0].veloc1;
        Rad1 = (MR_Float) records[I0].rad1;
        X1 = (MR_Float) records[I0].x1;
        Y1 = (MR_Float) records[I0].y1;
        I1 = I0 + 1;
    } else {
        Ok = MR_NO;
        T = (MR_Float) -1.0;
        Agent0 = MR_make_string_const("""");
        Veloc0 = (MR_Float) -1.0;
        Rad0 = (MR_Float) -1.0;
        X0 = (MR_Float) -1.0;
        Y0 = (MR_Float) -1.0;
        Agent1 = MR_make_string_const("""");
        Veloc1 = (MR_Float) -1.0;
        Rad1 = (MR_Float) -1.0;
        X1 = (MR_Float) -1.0;
        Y1 = (MR_Float) -1.0;
    }
").

%-----------------------------------------------------------------------------%

:- pred simple_init_obs(list(obs_msg)::uo) is det.

simple_init_obs(ObsMsgs1) :-
    solutions((pred(ObsMsg::out) is nondet :-
        obs(Obs),
        ObsMsg = obs_msg(Obs)
    ), ObsMsgs0),
    copy(ObsMsgs0, ObsMsgs1).


:- pred simple_next_obs(obs_msg::out, list(obs_msg)::di, list(obs_msg)::uo) is det.

simple_next_obs(P, [P|Ps], Ps).
simple_next_obs(end_of_obs, [], []).

%-----------------------------------------------------------------------------%

:- pred map0_io((pred(T1, io, io)::in(pred(in, di, uo) is det)),
               list(T1)::in, io::di, io::uo) is det.

map0_io(_, [], !IO).
map0_io(P, [X | Xs], !IO) :- P(X, !IO), map0_io(P, Xs, !IO).


:- type init_obs(T) == (pred(T)).
:- inst init_obs == (pred(uo) is det).
:- type next_obs(T) == (pred(obs_msg, T, T)).
:- inst next_obs == (pred(out, di, uo) is det).


:- pred match_in_prog(prog(prim, stoch, procedure)::in, prim::out) is nondet.

match_in_prog(seq(P1, P2), M) :-
    match_in_prog(P1, M) ;
    match_in_prog(P2, M).
match_in_prog(non_det(P1, P2), M) :-
    match_in_prog(P1, M) ;
    match_in_prog(P2, M).
match_in_prog(conc(P1, P2), M) :-
    match_in_prog(P1, M) ;
    match_in_prog(P2, M).
match_in_prog(star(P), M) :-
    match_in_prog(P, M).
match_in_prog(pseudo_atom(complex(P)), M) :-
    match_in_prog(P, M).
match_in_prog(pseudo_atom(atom(prim(A))), M) :-
    A = match(_, _, _, _, _),
    A = M.


:- pred match_in_sit(sit(prim)::in, prim::out) is nondet.

match_in_sit(do(A, S), M) :-
    (   A = match(_, _, _, _, _), A = M
    ;   match_in_sit(S, M) ).


:- type s_phase ---> running ; finishing ; finished ; failed.
:- type s_state ---> s_state(conf(prim, stoch, procedure), s_phase).


:- pred merge_and_trans_loop(next_obs(T)::in(next_obs),
                             s_state::in,
                             s_state::out,
                             T::di, T::uo) is det.

merge_and_trans_loop(NextObs, !State, !ObsGenState) :-
    merge_and_trans(NextObs, !State, !ObsGenState, Cont),
    (   if      Cont = yes
        then    merge_and_trans_loop(NextObs, !State, !ObsGenState)
        else    true
    ).


:- pred merge_and_trans(next_obs(T)::in(next_obs),
                        s_state::in,
                        s_state::out,
                        T::di, T::uo,
                        bool::out) is det.

merge_and_trans(NextObs,
                s_state(conf(P, S), !.Phase),
                s_state(conf(P2, S2), !:Phase),
                !ObsGenState,
                Continue) :-
    (   if      !.Phase \= finishing,
                match_count(P) < cars.lookahead(S)
        then    NextObs(ObsMsg, !ObsGenState)
        else    ObsMsg = end_of_obs
    ),
    P0 = ( if ObsMsg = obs_msg(Obs) then append_obs(P, Obs) else P ),
    S0 = ( if ObsMsg = init_msg(Map, T) then do(init_env(T, Map), S) else S ),
    (   if
            !.Phase \= finishing,
            match_count(P0) < cars.lookahead(S)
        then
            Continue = yes,
            P2 = P0,
            S2 = S0,
            !:Phase = ( if ObsMsg = end_of_obs then finishing else running )
        else if
            final(remove_match_sequence(P), S),
            last_action_covered_by_match(S)
        then
            Continue = no,
            P2 = P0,
            S2 = S0,
            !:Phase = finished
        else if
            trans(P0, S0, P1, S1)
        then
            Continue = yes,
            P2 = P1,
            S2 = S1,
            !:Phase = !.Phase
        else
            Continue = no,
            P2 = P0,
            S2 = S0,
            !:Phase = failed
    ).

%-----------------------------------------------------------------------------%

:- pred run_concurrently_par_conj(int::in,
                                  pred(int, s_state)::in(pred(in, out) is det),
                                  list(s_state)::out) is det.

run_concurrently_par_conj(I, P, Rs) :-
    (   if      I > 0
        then    ( P(I, R) & run_concurrently_par_conj(I - 1, P, Rs0) ),
                Rs = [R | Rs0]
        else    Rs = []
    ).


:- pred run_sequentially(int::in,
                         pred(int, s_state)::in(pred(in, out) is det),
                         list(s_state)::out) is det.

run_sequentially(I, P, Rs) :-
    (   if      I > 0
        then    P(I, R), run_sequentially(I - 1, P, Rs0),
                Rs = [R | Rs0]
        else    Rs = []
    ).


:- pred init_vars(int::in, list(mvar(T))::out, io::di, io::uo) is det.

init_vars(N, Vs, !IO) :-
    if      N > 0
    then    Vs = [V | Vs0],
            init(V, !IO),
            init_vars(N - 1, Vs0, !IO)
    else    Vs = [].


:- pred take_vars(list(mvar(T))::in, list(T)::out, io::di, io::uo) is det.

take_vars([], [], !IO).
take_vars([V | Vs], [R | Rs], !IO) :-
    take(V, R, !IO),
    take_vars(Vs, Rs, !IO).


:- pred run_concurrently_thread(int::in,
                                list(mvar(s_state))::in,
                                pred(int, s_state)::in(pred(in, out) is det),
                                io::di,
                                io::uo) is cc_multi.

run_concurrently_thread(_, [], _, !IO).
run_concurrently_thread(I, [V | Vs], P, !IO) :-
    spawn((pred(IO0::di, IO1::uo) is cc_multi :-
        P(I, R),
        put(V, R, IO0, IO1)
    ), !IO),
    run_concurrently_thread(I - 1, Vs, P, !IO).


:- pred run_concurrently(int::in,
                         pred(int, s_state)::in(pred(in, out) is det),
                         list(s_state)::out,
                         io::di, io::uo) is cc_multi.

%run_concurrently(N, P, Rs, !IO) :-
%    run_concurrently_par_conj(N, P, Rs).
%run_concurrently(N, P, Rs, !IO) :-
%    run_sequentially(N, P, Rs).
run_concurrently(N, P, Rs, !IO) :-
    init_vars(N, Vs, !IO),
    run_concurrently_thread(N, Vs, P, !IO),
    take_vars(Vs, Rs, !IO).

%-----------------------------------------------------------------------------%

:- pred planrecog(int::in,
                  init_obs(T)::in(init_obs),
                  next_obs(T)::in(next_obs),
                  prog(prim, stoch, procedure)::in,
                  list(s_state)::out,
                  io::di, io::uo) is cc_multi.

planrecog(ThreadCount, InitObs, NextObs, Prog, Results, !IO) :-
    Thread = (pred(I::in, R::out) is det :-
        InitialState = s_state(conf(Prog, do(seed(I), s0)), running),
        InitObs(InitialObsGenState),
        merge_and_trans_loop(NextObs, InitialState, R, InitialObsGenState, _)
    ),
    run_concurrently(ThreadCount, Thread, Results, !IO).

%-----------------------------------------------------------------------------%

:- pred exec(io::di, io::uo) is det.

exec(!IO) :-
    P = match_prog // p(cruise(a)) // p(overtake(b, a)),
    write(P, !IO), nl(!IO),
    C = init(P),
    write_string("initial situation", !IO), nl(!IO),
    (   if      S1 = sit(C),
                VG = vargen(S1),
                Cs = constraints(S1),
                solve(VG, Cs, min(variable_sum(VG)), Map, _Val)
        then    print_sit_info(stdout_stream, Map, s0, !IO), nl(stdout_stream, !IO)
        else    true
    ),
    exec(C, _, !IO).

:- pred exec(conf(prim, stoch, procedure)::in,
             conf(prim, stoch, procedure)::out,
             io::di, io::uo) is det.

exec(!C, !IO) :-
    %read_line_as_string(Line, !IO),
    (   if      final(!.C)
        then    write_string("finished", !IO), nl(!IO)
        else if %Line \= ok("q"),
                trans(!C),
                S1 = sit(!.C),
                VG = vargen(S1),
                Cs = constraints(S1),
                solve(VG, Cs, min(variable_sum(VG)), Map, _Val)
        then    %print_sit(Map, sit(!.C), !IO), nl(!IO),
                (   if      sit(!.C) = do(match(ObsT, _, _, _, _), _),
                            obs(ObsT, _, _, _, b, ObsX, ObsY)
                    then    print_sit_info(stdout_stream, Map, sit(!.C), !IO), nl(!IO),
                            format("obs[%f] = (%.1f, %.1f)\n\n\n",
                                   [f(ObsT), f(ObsX), f(ObsY)], !IO)
                    else    true
                ),
                %write(constraints(sit(!.C)), !IO), nl(!IO),
                exec(!C, !IO)
        else    write(rest(!.C), !IO), nl(!IO),
                write_string("stopped", !IO), nl(!IO),
%/*
                solutions((pred(X::out) is nondet :-
                    next2(rest(!.C), X, Y),
                    trace [io(!SubIO)] (
                        write(X, !SubIO), nl(!SubIO),
                        write(Y, !SubIO), nl(!SubIO),
                        (   if      X = stoch(B), cars.random_outcome(B, A, sit(!.C))
                            then    write_string("outcome ", !SubIO), write(A, !SubIO), nl(!SubIO),
                                    (   if      cars.poss(A, _, sit(!.C))
                                        then    write_string("possible!!", !SubIO), nl(!SubIO)
                                        else    write_string("impossible!!", !SubIO), nl(!SubIO)
                                    )
                            else    true
                        ),
                        nl(!SubIO)
                    )
                ), _),
%*/
                true
    ).

%-----------------------------------------------------------------------------%

:- func agent_to_string(agent) = string is det.

agent_to_string(a) = "a".
agent_to_string(b) = "b".


:- func string_to_agent(string) = agent is det.

string_to_agent(S) = A :-
    if      S = agent_to_string(a)
    then    A = a
    else if S = agent_to_string(b)
    then    A = b
    else    error("string_to_agent/1: conversion failed").


:- func lane_to_string(lane) = string is det.

lane_to_string(left) = "left".
lane_to_string(right) = "right".


:- pred print_action(output_stream::in,
                      assoc_list(var, number)::in, prim::in,
                     io::di, io::uo) is det.

print_action(Stream, Map, set_veloc(A, Mps, Tol, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "set_veloc(%s, %f, %f, %f)\n",
           [s(agent_to_string(A)), f(Mps), f(Tol), f(T)], !IO).
print_action(Stream, Map, set_yaw(A, L, Rad, Tol, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "set_yaw(%s, %s, %f, %f, %f)\n",
           [s(agent_to_string(A)), s(lane_to_string(L)),
            f(Rad), f(Tol), f(T)], !IO).
print_action(Stream, Map, wait_for(_, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "wait_for(..., %f)\n",
           [f(T)], !IO).
print_action(Stream, Map, match(OTime, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "match(%f, ..., %f)\n",
           [f(OTime), f(T)], !IO).
print_action(Stream, Map, eval(_, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format(Stream, "eval(..., %f)\n",
           [f(T)], !IO).
print_action(Stream, _, A @ init_env(_, _), !IO) :-
    write(Stream, A, !IO), nl(Stream, !IO).
print_action(Stream, _, seed(Seed), !IO) :-
    format(Stream, "seed(%d)\n",
           [i(Seed)], !IO).


:- pred print_sit(assoc_list(var, float)::in, sit(prim)::in,
                  io::di, io::uo) is det.

print_sit(Map, S, !IO) :- print_sit_2(stdout_stream, "", Map, S, 1, _, !IO).


:- pred print_sit(output_stream::in,
                  string::in,
                  assoc_list(var, float)::in, sit(prim)::in,
                  io::di, io::uo) is det.

print_sit(Stream, Prefix, Map, S, !IO) :-
    print_sit_2(Stream, Prefix, Map, S, 1, _, !IO).


:- pred print_sit_2(output_stream::in,
                    string::in,
                    assoc_list(var, float)::in, sit(prim)::in,
                    int::in, int::out, io::di, io::uo) is det.

print_sit_2(_, _, _, s0, !N, !IO).
print_sit_2(Stream, Prefix, Map, do(A, S), !.N, !:N, !IO) :-
    print_sit_2(Stream, Prefix, Map, S, !N, !IO),
    write_string(Stream, Prefix, !IO),
    write_string(Stream, " ", !IO),
    write(Stream, !.N, !IO),
    write_string(Stream, ": ", !IO),
    !:N = !.N + 1,
    print_action(Stream, Map, A, !IO).


:- pred print_sit_with_info(assoc_list(var, number)::in, sit(prim)::in,
                            io::di, io::uo) is det.

print_sit_with_info(Map, S, !IO) :-
    print_sit_with_info(stdout_stream, Map, S, !IO).


:- pred print_sit_with_info(output_stream::in,
                            assoc_list(var, number)::in, sit(prim)::in,
                            io::di, io::uo) is det.

print_sit_with_info(Stream, Map, s0, !IO) :-
    write_string(Stream, "initial situation", !IO), nl(!IO),
    print_sit_info(Stream, Map, s0, !IO),
    nl(Stream, !IO).
print_sit_with_info(Stream, Map, S1 @ do(A, S), !IO) :-
    print_sit_with_info(Stream, Map, S, !IO),
    print_action(Stream, Map, A, !IO),
    print_sit_info(Stream, Map, S1, !IO),
    nl(Stream, !IO).


:- pred print_sit_info(assoc_list(var, number)::in, sit(prim)::in,
                       io::di, io::uo) is det.

print_sit_info(Map, S, !IO) :- print_sit_info(stdout_stream, Map, S, !IO).


:- pred print_sit_info(output_stream::in,
                       assoc_list(var, number)::in, sit(prim)::in,
                       io::di, io::uo) is det.

print_sit_info(Stream, Map, S, !IO) :-
    E = ( func(T) = eval_float(Map, T) ),
    format(Stream, "veloc(b, S) = %.1f\n", [f(veloc(b, S))], !IO),
    format(Stream, "yaw(b, S) = %.1f\n", [f(yaw(b, S))], !IO),
    format(Stream, "start(S) = %.1f\n", [f(E(start(S)))], !IO),
    format(Stream, "x(b, S) = %.1f\n", [f(E(x(b, S)(start(S))))], !IO),
    format(Stream, "y(b, S) = %.1f\n", [f(E(y(b, S)(start(S))))], !IO),
    format(Stream, "x_tol(b, S) = %.1f\n", [f(x_tol(b, S))], !IO),
    format(Stream, "y_tol(b, S) = %.1f\n", [f(y_tol(b, S))], !IO),
    format(Stream, "now(S) = %.1f\n", [f(E(now(S)(start(S))))], !IO),
    nl(Stream, !IO),

    %Time = constant(13.132000),
    %wrt("x(b, S)(T) = ", E(x(b, S)(Time)), !IO),
    %wrt("y(b, S)(T) = ", E(y(b, S)(Time)), !IO),

    %(   if      solve(vargen(S), filter_empty_cstrs(on_right_lane(b)(start(S), S)))
    %    then    write_string("on_right_lane(b) holds", !IO)
    %    else    write_string("on_right_lane(b) holds not", !IO)
    %), nl(!IO),
    %wrt("on_right_lane = ", filter_empty_cstrs(on_right_lane(b)(start(S), S)), !IO),
    %(   if      solve(vargen(S), filter_empty_cstrs(on_left_lane(b)(start(S), S)))
    %    then    write_string("on_left_lane(b) holds", !IO)
    %    else    write_string("on_left_lane(b) holds not", !IO)
    %), nl(!IO),
    %wrt("on_left_lane = ", filter_empty_cstrs(on_left_lane(b)(start(S), S)), !IO),
    true.


:- pred open_next_file(string::in, output_stream::out, io::di, io::uo) is det.

open_next_file(Format, Stream, !IO) :-
    open_next_file_2(Format, 0, 999, Stream, !IO).


:- pred open_next_file_2(string::in, int::in, int::in, output_stream::out,
                         io::di, io::uo) is det.

open_next_file_2(Format, I, N, Stream, !IO) :-
    if      I > N
    then    error("cannot open file with pattern "++ Format)
    else    Filename = format(Format, [i(I)]),
            open_input(Filename, TestRes, !IO),
            (   if      TestRes = ok(TestStream)
                then    close_input(TestStream, !IO),
                        open_next_file_2(Format, I + 1, N, Stream, !IO)
                else    open_output(Filename, Res, !IO),
                        (   if      Res = ok(Stream0)
                            then    Stream = Stream0
                            else    error("cannot open non-existing file " ++
                                          "with pattern " ++ Format ++
                                          " for " ++ int_to_string(I))
                        )
            ).


:- pred draw_trace(assoc_list(var, number)::in, sit(prim)::in,
                   io::di, io::uo) is det.

draw_trace(Map, S, !IO) :-
    open_next_file("traces/%04d.dat", Stream, !IO),
    print_sit(Stream, "# ", Map, S, !IO),
    draw_trace_2(Stream, Map, S, !IO),
    close_output(Stream, !IO).


:- pred draw_trace_2(output_stream::in,
                     assoc_list(var, number)::in, sit(prim)::in,
                     io::di, io::uo) is det.


draw_trace_2(Stream, Map, S, !IO) :-
    Agent = b,
    (   if   S = do(A, S0), A \= init_env(_, _)
        then draw_trace_2(Stream, Map, S0, !IO)
        else format(Stream, "time     xobs     yobs     xmod     ymod      xlo      ylo      xhi      yhi\n", [], !IO)
    ),
    (   (   if      S = do(match(_, Obs, _, _, _), _)
            then    (   if      Obs = {_, Agent, X0, Y0, _, _, _}
                        then    ObsX = format("%7.3f", [f(X0)]),
                                ObsY = format("%7.3f", [f(Y0)])
                        else if Obs = {_, _, _, _, Agent, X0, Y0}
                        then    ObsX = format("%7.3f", [f(X0)]),
                                ObsY = format("%7.3f", [f(Y0)])
                        else    error("invalid observation does not contain driver")
                    )
            else    ObsX = "    NaN", ObsY = "    NaN"
        ),
        E = ( func(T) = eval_float(Map, T) ),
        Time = E(start(S)),
        ModX = E(x(Agent, S)(start(S))),
        ModY = E(y(Agent, S)(start(S))),
        ModXTol = x_tol(Agent, S),
        ModYTol = y_tol(Agent, S),
        format(Stream, "%7.3f  %s  %s  %7.3f  %7.3f  %7.3f  %7.3f  %7.3f  %7.3f\n",
                       [f(Time), s(ObsX), s(ObsY), f(ModX), f(ModY),
                        f(ModX - ModXTol), f(ModY - ModYTol),
                        f(ModX + ModXTol), f(ModY + ModYTol)], !IO)
    ).


:- pred draw_traces_incl_subsits(assoc_list(var, number)::in, sit(prim)::in,
                   io::di, io::uo) is det.

draw_traces_incl_subsits(_, s0, !IO).
draw_traces_incl_subsits(Map, S1 @ do(_, S), !IO) :-
    draw_traces_incl_subsits(Map, S, !IO),
    draw_trace(Map, S1, !IO).


:- pred wrt(string::in, T::in, io::di, io::uo) is det.

wrt(S, T, !IO) :- write_string(S, !IO), write(T, !IO), nl(!IO).

%-----------------------------------------------------------------------------%

:- import_module pair.

% Solve the maze using a program:
%    (up | down | left | right)*
main(!IO) :-
    %test(init_vargen, _, !IO).
/*
    deg2rad(10.0, Rad),
    P = b(set_yaw_st(a, Rad)) `;`
        b(set_veloc_st(a, 15.0)) `;`
        a(wait_for( func(T, S) = [
            x(a, S)(T) `>=` constant(rat(10)),
            y(a, S)(T) `>=` constant(rat(5))
        ], no, [], notime)) `;`
        b(set_yaw_st(a, 0.0)) `;`
        a(wait_for( func(T, S) = [
            x(a, S)(T) `>=` constant(rat(900))
        ], no, [], notime)),
*/

/*
    times(Tms0, !IO),
    exec(!IO),
    times(Tms1, !IO),
    format("usertime = %f\n", [f(usertime(Tms0, Tms1))], !IO),
    format("systime = %f\n", [f(systime(Tms0, Tms1))], !IO),
/*
*/

    times(Tms2, !IO),
    Prog = p(cruise(a)) // p(overtake(b, a)),
    %planrecog(1, simple_init_obs, simple_next_obs, Prog, Results, !IO),
    planrecog(4, input_init_obs, input_next_obs, Prog, Results, !IO),
    times(Tms3, !IO),
    map0_io((pred(s_state(conf(P, S), R)::in, IO0::di, IO1::uo) is det :-
        some [!SubIO] (
            IO0 = !:SubIO,
            write(R, !SubIO), nl(!SubIO),
            (   if      solve(vargen(S), constraints(S), Map, _Val)
                then    print_sit(Map, S, !SubIO),
                        print_sit_info(Map, S, !SubIO),
                        (   if      R = finished
                            then    %draw_traces_incl_subsits(Map, S, !SubIO)
                                    draw_trace(Map, S, !SubIO)
                            else    true
                        )
                else    write_string("solving failed\n", !SubIO)
            ),
            write_string("Remaining program: ", !SubIO),
            write(P, !SubIO), nl(!SubIO),
            nl(!SubIO),
            IO1 = !.SubIO
        )
    ), Results, !IO),
    (   if      Results \= []
        then    foldl((pred(s_state(_, R)::in, {N, M}::in, {N1, M1}::out) is det :-
                    if      R = finished
                    then    N1 = N + 1, M1 = M + 1
                    else    N1 = N,     M1 = M + 1
                ), Results, {0, 0}, {Finished, Total}),
                format("percentage = %d / %d = %.2f\n",
                       [i(Finished), i(Total),
                        f(float(Finished) / float(Total))], !IO)
        else    format("percentage = nan\n", [], !IO)
    ),
    format("usertime = %f\n", [f(usertime(Tms2, Tms3))], !IO),
    format("systime = %f\n", [f(systime(Tms2, Tms3))], !IO),

/*
*/
/*
    P = match_prog // p(cruise(a)),% // p(overtake(b, a)),
    (   if      do(P, s0, S1),
                VG = vargen(S1),
                Cs = constraints(S1),
                solve(VG, Cs, min(variable_sum(VG)), Map, Val)
        then    nl(!IO), nl(!IO), write_string("  ---", !IO), nl(!IO), nl(!IO),
                print_sit_with_info(Map, S1, !IO),
                write_string("  ---", !IO), nl(!IO), nl(!IO),
                write(to_float(Val), !IO), nl(!IO),
                write(Map, !IO), nl(!IO),
                nl(!IO),
                write(constraints(S1), !IO), nl(!IO)
        else    write_string("failed", !IO), nl(!IO)
    ).
/*
*/
    true.

%-----------------------------------------------------------------------------%
:- end_module cars.
%-----------------------------------------------------------------------------%

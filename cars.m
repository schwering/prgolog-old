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

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module prgolog.fluent.
:- import_module prgolog.nice.
:- import_module assoc_list.
:- import_module int.
:- import_module float.
:- import_module list.
:- import_module math.
:- import_module maybe.
:- use_module random.
:- import_module string.
:- import_module solutions.
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
:- type prim --->
        set_veloc(agent, mps, mps, maybe(random.supply),
                  maybe(vargen), list(constraint), time)
    ;   set_yaw(agent, lane, rad, rad, maybe(random.supply), maybe(vargen),
                list(constraint), time)
    ;   wait_for(ccformula(prim), maybe(vargen), list(constraint), time)
    ;   match(s, ccformula(prim), maybe(vargen), list(constraint), time)
    ;   eval(ccformula(prim), maybe(vargen), list(constraint), time).
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

%start(s0) = constant(zero).
start(s0) = constant(T) :- initial_time(T).
start(do(A, _)) = T :-
    (   A = set_veloc(_, _, _, _, _, _, T)
    ;   A = set_yaw(_, _, _, _, _, _, _, T)
    ;   A = wait_for(_, _, _, T)
    ;   A = match(_, _, _, _, T)
    ;   A = eval(_, _, _, T)
    ).

%-----------------------------------------------------------------------------%

:- func now(sit(prim)::in) = (tfunc::out).

now(_) = ( func(T) = T ).

%-----------------------------------------------------------------------------%

:- func random_supply(sit(prim)::in) = (random.supply::out) is det.

random_supply(s0) = RS :-
    random.init(3, RS).
random_supply(do(A, S)) = RS :-
    if      (   A = set_veloc(_, _, _, yes(RS0), _, _, _)
            ;   A = set_yaw(_, _, _, _, yes(RS0), _, _, _) )
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
    ).

%-----------------------------------------------------------------------------%

:- func veloc(sit(prim)::in) = (mps::out) is det.

veloc(s0) = 0.0.
veloc(do(A, S)) = V :-
    if      A = set_veloc(_, V0, _, _, _, _, _)
    then    V = V0
    else    V = veloc(S).

%-----------------------------------------------------------------------------%

:- func yaw(sit(prim)::in) = (mps::out) is det.

yaw(s0) = 0.0.
yaw(do(A, S)) = Rad :-
    if      A = set_yaw(_, _, Rad0, _, _, _, _, _)
    then    Rad = Rad0
    else    Rad = yaw(S).

%-----------------------------------------------------------------------------%

:- func x(agent::in, sit(prim)::in) = (tfunc::out) is det.

%x(_, s0) = ( func(_) = constant(zero) ).
x(Agent, s0) = ( func(_) = constant(X) ) :-
    initial(Agent, X, _).
x(Agent, do(A, S)) = X :-
    if      (   A = set_veloc(Agent, Veloc, _, _, _, _, T0), Rad = yaw(S)
            ;   A = set_yaw(Agent, _, Rad, _, _, _, _, T0), Veloc = veloc(S)
            )
    then    X = ( func(T) = cos(Rad) * Veloc * (T - T0) +
                            x(Agent, S)(T0) )
    else    X = x(Agent, S).

%-----------------------------------------------------------------------------%

:- func y(agent::in, sit(prim)::in) = (tfunc::out) is det.

%y(_, s0) = ( func(_) = constant(zero) ).
y(Agent, s0) = ( func(_) = constant(Y) ) :-
    initial(Agent, _, Y).
y(Agent, do(A, S)) = Y :-
    if      (   A = set_veloc(Agent, Veloc, _, _, _, _, T0), Rad = yaw(S)
            ;   A = set_yaw(Agent, _, Rad, _, _, _, _, T0), Veloc = veloc(S)
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

poss(match(OT, OF, no, [], notime),
     match(OT, OF, yes(VG), Cs0, T),
     S) :-
    T = new_variable(vargen(S), VG),
    % XXX remove third constraint
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
    Tol = 0.9 + Yaw * 4.0,
    RS0 = random_supply(S),
    random_lognormal(1.0, 1.0, _, _Tol, RS0, RS1).

%-----------------------------------------------------------------------------%

:- func lookahead(sit(prim)) = lookahead is det.

lookahead(_S) = 6.

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
            %( %b(set_yaw_st(Agent, right, deg2rad(14.0))) or
              %b(set_yaw_st(Agent, right, deg2rad(12.0))) or
              %b(set_yaw_st(Agent, right, deg2rad(10.0))) or
              b(set_yaw_st(Agent, right, deg2rad(8.0)))% or
              %b(set_yaw_st(Agent, right, deg2rad(6.0))) ) `;`
            %nil% a(eval(on_right_lane(Agent), no, [], notime))
        ) `;`
        p(straight_left(Agent)).

proc(right_lane_change(Agent), P) :-
    P = atomic(
            ( b(set_yaw_st(Agent, left, deg2rad(-14.0))) or
              %b(set_yaw_st(Agent, left, deg2rad(-12.0))) or
              %b(set_yaw_st(Agent, left, deg2rad(-10.0))) or
              %b(set_yaw_st(Agent, left, deg2rad(-8.0))) or
              b(set_yaw_st(Agent, left, deg2rad(-6.0))) ) `;`
            nil% a(eval(on_right_lane(Agent), no, [], notime))
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

:- pred initial_time(s::out) is det.

initial_time(5.594000).

:- pred initial(agent::in, m::out, m::out) is det.

initial(a, 34.776825, -2.999933).
initial(b, 6.304431, -3.273941).

:- pred obs(s::out, agent::out, m::out, m::out,
                    agent::out, m::out, m::out) is multi.

obs(5.594000,  a, 34.776825, -2.999933,  b, 6.304431, -3.273941).
obs(6.100000,  a, 42.410252, -2.999933,  b, 16.886641, -3.226734).
obs(6.606000,  a, 50.046276, -2.999933,  b, 27.465494, -3.191746).
obs(7.112000,  a, 57.682270, -2.999933,  b, 38.043564, -3.213415).
obs(7.618000,  a, 65.313354, -2.999933,  b, 48.628551, -3.218992).
obs(8.132000,  a, 73.062225, -2.999933,  b, 59.107368, -1.065044).
obs(8.632000,  a, 80.600098, -2.999933,  b, 69.129898, 1.880987).
obs(9.132000,  a, 88.137970, -2.999933,  b, 79.493713, 2.901567).
obs(9.632000,  a, 95.675766, -2.999933,  b, 89.940277, 2.991667).
obs(10.132000, a, 103.213608, -2.999933, b, 100.390549, 3.025797).
obs(10.632000, b, 110.836411, 3.002779,  a, 110.751450, -2.999933).
obs(11.132000, b, 121.284592, 2.924112,  a, 118.289291, -2.999933).
obs(11.632000, b, 131.733932, 2.790201,  a, 125.827133, -2.999933).
obs(12.132000, b, 142.175156, 2.600296,  a, 133.364975, -2.999933).
obs(12.632000, b, 152.542404, 1.616369,  a, 140.902817, -2.999933).
/*
obs(13.132000, b, 162.487106, -1.544439, a, 148.440659, -2.999933).
obs(13.632000, b, 172.840927, -2.558414, a, 155.988510, -2.999933).
obs(14.132000, b, 183.293320, -2.678141, a, 163.527390, -2.999933).
obs(14.632000, b, 193.741440, -2.689064, a, 171.065231, -2.999933).
obs(15.132000, b, 204.195236, -2.745777, a, 178.591293, -2.999933).
obs(15.632000, b, 214.648361, -2.857815, a, 186.113022, -2.999933).
*/

:- pred obs(prim::out) is multi.

obs(match(OT, OF, no, [], notime)) :-
    obs(OT, A0, X0, Y0, A1, X1, Y1),
    C = ( func(F) = constant(F) ),
    OF = ( func(T, S) = [
        C(X0 - x_tol(A0, S)) `=<` x(A0, S)(T), x(A0, S)(T) `=<` C(X0 + x_tol(A0, S)),
        C(Y0 - y_tol(A0, S)) `=<` y(A0, S)(T), y(A0, S)(T) `=<` C(Y0 + y_tol(A0, S)),
        C(X1 - x_tol(A1, S)) `=<` x(A1, S)(T), x(A1, S)(T) `=<` C(X1 + x_tol(A1, S)),
        C(Y1 - y_tol(A1, S)) `=<` y(A1, S)(T), y(A1, S)(T) `=<` C(Y1 + y_tol(A1, S))
    ] ).

:- func obs_prog = prog(prim, stoch, procedure) is det.

obs_prog = Prog :-
    solutions(obs, Actions),
    Progs = list.map(( func(A) = pseudo_atom(atom(prim(A))) ), Actions),
    Prog = list.foldr(( func(P1, P2) = seq(P1, P2) ), Progs, nil).

%-----------------------------------------------------------------------------%

:- func agent_to_string(agent) = string is det.

agent_to_string(a) = "a".
agent_to_string(b) = "b".


:- func lane_to_string(lane) = string is det.

lane_to_string(left) = "left".
lane_to_string(right) = "right".


:- pred print_action(assoc_list(var, number)::in, prim::in,
                     io::di, io::uo) is det.

print_action(Map, set_veloc(A, Mps, Tol, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format("set_veloc(%s, %f, %f, %f)\n",
           [s(agent_to_string(A)), f(Mps), f(Tol), f(T)], !IO).
print_action(Map, set_yaw(A, L, Rad, Tol, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format("set_yaw(%s, %s, %f, %f, %f)\n",
           [s(agent_to_string(A)), s(lane_to_string(L)),
            f(Rad), f(Tol), f(T)], !IO).
print_action(Map, wait_for(_, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format("wait_for(..., %f)\n",
           [f(T)], !IO).
print_action(Map, match(OTime, _, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format("match(%f, ..., %f)\n",
           [f(OTime), f(T)], !IO).
print_action(Map, eval(_, _, _, Time), !IO) :-
    T = eval_float(Map, Time),
    format("eval(..., %f)\n",
           [f(T)], !IO).


:- pred print_sit(assoc_list(var, float)::in, sit(prim)::in,
                  io::di, io::uo) is det.

print_sit(Map, S, !IO) :- print_sit_2(Map, S, 1, _, !IO).


:- pred print_sit_2(assoc_list(var, float)::in, sit(prim)::in,
                    int::in, int::out, io::di, io::uo) is det.

print_sit_2(_, s0, !N, !IO).
print_sit_2(Map, do(A, S), !.N, !:N, !IO) :-
    print_sit_2(Map, S, !N, !IO),
    write_string(" ", !IO), write(!.N, !IO), write_string(": ", !IO),
    !:N = !.N + 1,
    print_action(Map, A, !IO).


:- pred print_sit_with_info(assoc_list(var, number)::in, sit(prim)::in, io::di, io::uo) is det.

print_sit_with_info(Map, s0, !IO) :-
    write_string("initial situation", !IO), nl(!IO),
    print_sit_info(Map, s0, !IO),
    nl(!IO).
print_sit_with_info(Map, S1 @ do(A, S), !IO) :-
    print_sit_with_info(Map, S, !IO),
    print_action(Map, A, !IO),
    print_sit_info(Map, S1, !IO),
    nl(!IO).


:- pred print_sit_info(assoc_list(var, number)::in, sit(prim)::in,
                       io::di, io::uo) is det.

print_sit_info(Map, S, !IO) :-
    E = ( func(T) = eval_float(Map, T) ),
    wrt("veloc(S) = ", veloc(S), !IO),
    wrt("yaw(S) = ", yaw(S), !IO),
    wrt("start(S) = ", E(start(S)), !IO),
    wrt("x(b, S) = ", E(x(b, S)(start(S))), !IO),
    wrt("y(b, S) = ", E(y(b, S)(start(S))), !IO),
    wrt("x_tol(b, S) = ", x_tol(b, S), !IO),
    wrt("y_tol(b, S) = ", y_tol(b, S), !IO),
    wrt("now(S) = ", E(now(S)(start(S))), !IO),

    Time = constant(8.632000),
    wrt("x(b, S)(T) = ", E(x(b, S)(Time)), !IO),
    wrt("y(b, S)(T) = ", E(y(b, S)(Time)), !IO),
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

:- pred wrt(string::in, T::in, io::di, io::uo) is det.

wrt(S, T, !IO) :- write_string(S, !IO), write(T, !IO), nl(!IO).

%-----------------------------------------------------------------------------%

:- pred exec(io::di, io::uo) is det.

exec(!IO) :-
    %P = p(cruise(a)) // p(overtake(b, a)),
    P = obs_prog // p(cruise(a)) // p(overtake(b, a)),
    C = init(P),
    write_string("initial situation", !IO), nl(!IO),
    (   if      S1 = sit(C),
                VG = vargen(S1),
                Cs = constraints(S1),
                solve(VG, Cs, min(variable_sum(VG)), Map, _Val)
        then    print_sit_info(Map, s0, !IO), nl(!IO)
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
        then    print_sit(Map, sit(!.C), !IO), nl(!IO),
                print_sit_info(Map, sit(!.C), !IO), nl(!IO),
                %write(constraints(sit(!.C)), !IO), nl(!IO),
                exec(!C, !IO)
        else    write(rest(!.C), !IO), nl(!IO),
                write_string("stopped", !IO), nl(!IO),
%/*
                solutions((pred(X::out) is nondet :-
                    next2(rest(!.C), X, Y),
                    trace [io(!IO)] (
                        write(X, !IO), nl(!IO),
                        write(Y, !IO), nl(!IO),
                        (   if      X = stoch(B), cars.random_outcome(B, A, sit(!.C))
                            then    write_string("outcome ", !IO), write(A, !IO), nl(!IO),
                                    (   if      cars.poss(A, _, sit(!.C))
                                        then    write_string("possible!!", !IO), nl(!IO)
                                        else    write_string("impossible!!", !IO), nl(!IO)
                                    )
                            else    true
                        ),
                        nl(!IO)
                    )
                ), _),
%*/
                true
    ).

%-----------------------------------------------------------------------------%

:- use_module lpq.
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
*/
    exec(!IO).
/*
*/
/*
    P = obs_prog // p(cruise(a)),% // p(overtake(b, a)),
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

%-----------------------------------------------------------------------------%
:- end_module cars.
%-----------------------------------------------------------------------------%

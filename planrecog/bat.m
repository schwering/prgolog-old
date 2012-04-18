%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: bat.m.
% Main author: schwering.
%
% Basic action theory (BAT) for driving with two simple actions, set_yaw and
% set_veloc that control the steering and speed of the vehicle.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module bat.

:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module maybe.
:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module prgolog.nice.
:- use_module random.
:- import_module types.

:- include_module io_util.

%-----------------------------------------------------------------------------%

:- type agent_info ---> agent_info(mps, rad, m, m).
:- type prim --->
        set_veloc(agent, mps, mps, maybe(random.supply),
                  maybe(vargen), list(constraint), time)
    ;   set_yaw(agent, lane, rad, rad, maybe(random.supply), maybe(vargen),
                list(constraint), time)
    ;   wait_for(ccformula(prim), maybe(vargen), list(constraint), time)
    %;   match(s, ccformula(prim), maybe(vargen), list(constraint), time)
    ;   match(obs, maybe(vargen), list(constraint), time)
    ;   eval(ccformula(prim), maybe(vargen), list(constraint), time)
    ;   init_env(s, assoc_list(agent, agent_info))
    ;   seed(int).
:- type stoch --->  set_veloc_st(agent, mps)
                ;   set_yaw_st(agent, lane, rad).
:- type proc --->  straight_left(agent)
                ;  straight_right(agent)
                ;  left_lane_change(agent)
                ;  right_lane_change(agent)
                ;  cruise(agent)
                ;  overtake(agent, agent).

:- type sit == sit(prim).
:- type prog == prog(prim, stoch, proc).
:- type conf == conf(prim, stoch, proc).

%-----------------------------------------------------------------------------%

:- instance bat(bat.prim, bat.stoch, bat.proc).

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

:- pred poss(prim::in, prim::out, sit::in) is semidet.

%-----------------------------------------------------------------------------%

:- pred random_outcome(stoch::in, prim::out, sit::in) is det.

%-----------------------------------------------------------------------------%

:- func lookahead(sit) = lookahead is det.

%-----------------------------------------------------------------------------%

:- func reward(sit) = reward.
:- mode reward(in) = out is det.

%-----------------------------------------------------------------------------%

:- func match_count(prog) = int.

%-----------------------------------------------------------------------------%

:- pred proc(proc::in, prog::out) is det.

%-----------------------------------------------------------------------------%

:- func obs2ccformula(obs::in) = ({s, ccformula(prim)}::out) is det.

:- func obs2match(obs::in) = (prim::out) is det.

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
    ;   A = init_env(T0, _), T = constant(T0)
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
    ;   A = init_env(_, _), Cs = []
    ;   A = seed(_), Cs = []
    ).

%-----------------------------------------------------------------------------%

veloc(_, s0) = 0.0.
veloc(Agent, do(A, S)) = Veloc :-
    if      A = init_env(_, Map)
    then    agent_info(Veloc, _, _, _) = Map^det_elem(Agent)
    else if A = set_veloc(Agent, V0, _, _, _, _, _)
    then    Veloc = V0
    else    Veloc = veloc(Agent, S).

%-----------------------------------------------------------------------------%

yaw(_, s0) = 0.0.
yaw(Agent, do(A, S)) = Rad :-
    if      A = init_env(_, Map)
    then    agent_info(_, Rad, _, _) = Map^det_elem(Agent)
    else if A = set_yaw(Agent, _, Rad0, _, _, _, _, _)
    then    Rad = Rad0
    else    Rad = yaw(Agent, S).

%-----------------------------------------------------------------------------%

x(_, s0) = ( func(_) = constant(0.0) ).
%x(Agent, s0) = ( func(_) = constant(X) ) :- initial(Agent, X, _).
x(Agent, do(A, S)) = X :-
    if      A = init_env(_, Map)
    then    agent_info(_, _, X0, _) = Map^det_elem(Agent),
            X = ( func(_) = constant(X0) )
    else if (   A = set_veloc(Agent, Veloc, _, _, _, _, T0), Rad = yaw(Agent, S)
            ;   A = set_yaw(Agent, _, Rad, _, _, _, _, T0), Veloc = veloc(Agent, S)
            )
    then    X = ( func(T) = cos(Rad) * Veloc * (T - T0) +
                            x(Agent, S)(T0) )
    else    X = x(Agent, S).

%-----------------------------------------------------------------------------%

y(_, s0) = ( func(_) = constant(0.0) ).
%y(Agent, s0) = ( func(_) = constant(Y) ) :- initial(Agent, _, Y).
y(Agent, do(A, S)) = Y :-
    if      A = init_env(_, Map)
    then    agent_info(_, _, _, Y0) = Map^det_elem(Agent),
            Y = ( func(_) = constant(Y0) )
    else if (   A = set_veloc(Agent, Veloc, _, _, _, _, T0), Rad = yaw(Agent, S)
            ;   A = set_yaw(Agent, _, Rad, _, _, _, _, T0), Veloc = veloc(Agent, S)
            )
    then    Y = ( func(T) = sin(Rad) * Veloc * (T - T0) +
                            y(Agent, S)(T0) )
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

:- func filter_empty_cstrs(list(constraint)::in) = (list(constraint)::out) is det.

filter_empty_cstrs(Cs) = negated_filter(holds_trivially, Cs).


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

poss(match(Obs, no, [], T),
     match(Obs, yes(VG), Cs0, T),
     S) :-
    VG = vargen(S),
    {_, OF} = obs2ccformula(Obs),
    Cs0 = filter_empty_cstrs([T `>=` start(S), T `=` T] ++ OF(T, S)),
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

lookahead(_S) = 3.

%-----------------------------------------------------------------------------%

:- func new_lookahead(lookahead, atom(prim, stoch)) = lookahead is det.

new_lookahead(H, _C) = H - 1.

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

match_count(seq(P1, P2)) = match_count(P1) + match_count(P2).
match_count(non_det(P1, P2)) = min(match_count(P1), match_count(P2)).
match_count(conc(P1, P2)) = match_count(P1) + match_count(P2).
match_count(star(_)) = 0.
match_count(proc(_)) = 0.
match_count(nil) = 0.
match_count(pseudo_atom(complex(P))) = match_count(P).
match_count(pseudo_atom(atom(A))) =
    ( if A = prim(match(_, _, _, _)) then 1 else 0 ).

%-----------------------------------------------------------------------------%

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

:- instance bat(bat.prim, bat.stoch, bat.proc) where [
    pred(poss/3) is bat.poss,
    pred(random_outcome/3) is bat.random_outcome,
    func(reward/2) is bat.reward,
    func(lookahead/1) is bat.lookahead,
    func(new_lookahead/2) is bat.new_lookahead,
    pred(proc/2) is bat.proc
].

%-----------------------------------------------------------------------------%

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


obs2match(Obs) = match(Obs, no, [], constant(OT)) :-
    {OT, _} = obs2ccformula(Obs).

%-----------------------------------------------------------------------------%
:- end_module bat.
%-----------------------------------------------------------------------------%

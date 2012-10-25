%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.m.
% Main author: schwering.
%
% Basic action theory (BAT) for driving with two simple actions, set_yaw and
% set_veloc that control the steering and speed of the vehicle.
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.

:- interface.

:- import_module prgolog.
:- import_module prgolog.nice.

%-----------------------------------------------------------------------------%

:- type ntg == s.
:- type ttc == s.

:- type car ---> b ; c ; d.
:- type prim
    --->    wait(s)
    ;       accel(car, float)
    ;       lc(car, int)
    ;       sense(car, car, ntg, ttc).
:- type stoch == int.
:- type proc == int.

:- type sit == sit(prim).
:- type prog == prog(prim, stoch, proc).
:- type conf == conf(prim, stoch, proc).

%-----------------------------------------------------------------------------%

%:- instance bat(prim, stoch, proc).
%:- instance obs_bat(prim, stoch, proc, obs).
%:- instance pr_bat(prim, stoch, proc, obs, env).

%-----------------------------------------------------------------------------%

:- pred ntg(car, car, ntg, sit).
:- mode ntg(in, in, out, in) is semidet.

:- pred ttc(car, car, ttc, sit).
:- mode ttc(in, in, out, in) is semidet.

:- func lane(car::in, sit::in) = (int::out) is det.
:- func start(sit::in) = (s::out) is det.

%-----------------------------------------------------------------------------%

:- pred poss(prim::in, prim::out, sit::in) is semidet.

%-----------------------------------------------------------------------------%

%:- pred random_outcome(stoch::in, prim::out, sit::in) is det.

%-----------------------------------------------------------------------------%

%:- func lookahead(sit) = lookahead is det.

%-----------------------------------------------------------------------------%

%:- func reward(sit) = reward.
%:- mode reward(in) = out is det.

%-----------------------------------------------------------------------------%

%:- pred proc(proc::in, prog::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module prgolog.nice.
:- import_module solutions.

%-----------------------------------------------------------------------------%

:- func x(car) = (float).
:- mode x(in) = (out) is det.
:- mode x(out) = (out) is multi.

x(b) = 1.0.
x(c) = 5.0.
x(d) = 1.0.

:- func v(car) = (float).
:- mode v(in) = (out) is det.
:- mode v(out) = (out) is multi.

v(b) = 3.0.
v(c) = 2.0.
v(d) = 1.0.

:- pred car(car).
:- mode car(in) is det.
:- mode car(out) is multi.

car(b).
car(c).
car(d).

%-----------------------------------------------------------------------------%

:- func min({float, car}::in, {float, car}::in) = ({float, car}::out) is det.

min(E1 @ {Cost1, _}, E2 @ {Cost2, _}) = ( if Cost1 < Cost2 then E1 else E2 ).


:- func third_car_in_sight(car::in, car::in, sit::in) = (car::out) is semidet.

third_car_in_sight(B, D, S) = E :-
    Cars = solutions((pred(C::out) is nondet :- car(C), C \= B, C \= D)),
    map((pred(C::in, {Cost, C}::out) is semidet :-
        ttc(B, C, TTC_BC, S), TTC_BC \= 0.0,
        ttc(C, D, TTC_CD, S), TTC_CD \= 0.0,
        ntg(B, C, NTG_BC, S),
        ntg(C, D, NTG_CD, S),
        Cost = abs(NTG_BC + NTG_CD + TTC_BC + TTC_CD)
    ), Cars, [FirstCandidate|Candidates]),
    {_, E} = foldr(min, Candidates, FirstCandidate).

%-----------------------------------------------------------------------------%

ntg(B, D, R, s0) :-
    v(B) \= 0.0,
    R = (x(D) - x(B)) / v(B).
ntg(B, D, R, do(A, S)) :-
    (
        A = wait(T) ->
        (   if
                ttc(B, D, TTC_BD, S), TTC_BD \= 0.0
            then
                ntg(B, D, NTG_BD, S),
                R = NTG_BD - T * NTG_BD / TTC_BD
            else
                C = third_car_in_sight(B, D, S),
                ttc(B, C, TTC_BC, S), TTC_BC \= 0.0,
                ttc(C, D, TTC_CD, S), TTC_CD \= 0.0,
                ntg(B, C, NTG_BC, S),
                ntg(C, D, NTG_CD, S),
                R1 = NTG_BC - T * NTG_BC / TTC_BC,
                R2 = TTC_BC - T, R2 \= 0.0,
                R3 = NTG_CD - T * NTG_CD / TTC_CD,
                R = R1 + (1.0 - R1 / R2) * R3
        )
    ;
        A = accel(B, Q), Q \= 0.0 ->
        ntg(B, D, NTG_BD, S),
        R = 1.0 / Q * NTG_BD
    ;
        A = sense(B, D, NTG_BD, _) ->
        R = NTG_BD
    ;
        A = sense(D, B, NTG_DB, TTC_DB) ->
        NTG_DB \= TTC_DB,
        R = -1.0 / (1.0 - NTG_DB / TTC_DB) * NTG_DB
    /*  What to do when A = sense(_, _, _, _)?
     *  We could try to to do it transitively, but that probably wouldn't be
     *  intended if the preceding action was, e.g., sense(B, D, _, _).
     *  I guess this is related to the problem of deciding on a NTG/TTC in
     *  the presence of measuring inaccuracy. */
    ;
        A \= wait(_),
        A \= accel(B, _),
        A \= sense(B, _, _, _),
        A \= sense(_, B, _, _),
        ntg(B, D, R, S)
    ).

%-----------------------------------------------------------------------------%

ttc(B, D, R, s0) :-
    v(B) - v(D) \= 0.0,
    R = (x(D) - x(B)) / (v(B) - v(D)).
ttc(B, D, R, do(A, S)) :-
    (
        A = wait(T) ->
        ttc(B, D, TTC_BD, S),
        R = TTC_BD - T
    ;
        A = accel(B, Q) ->
        (   if
                ntg(B, D, NTG_BD, S), NTG_BD \= 0.0,
                ttc(B, D, TTC_BD, S), TTC_BD \= 0.0
            then
                (Q - 1.0) * TTC_BD / NTG_BD + 1.0 \= 0.0,
                R = 1.0 / ((Q - 1.0) * TTC_BD / NTG_BD + 1.0) * TTC_BD
            else
                C = third_car_in_sight(B, D, S),
                ntg(B, C, NTG_BC, S), NTG_BC \= 0.0,
                ntg(C, D, NTG_CD, S), NTG_CD \= 0.0,
                ttc(B, C, TTC_BC1, S), TTC_BC1 \= 0.0,
                ttc(C, D, TTC_CD, S), TTC_CD \= 0.0,
                (Q - 1.0) * TTC_BC1 / NTG_BC + 1.0 \= 0.0,
                TTC_BC = 1.0 / ((Q - 1.0) * TTC_BC1 / NTG_BC + 1.0) * TTC_BC1,
                R1 = (TTC_CD * NTG_BC) /
                     (NTG_CD * TTC_BC + TTC_CD * NTG_BC - NTG_CD * NTG_BC),
                R2 = (TTC_BC * NTG_CD - NTG_BC * NTG_CD) /
                     (NTG_BC * TTC_CD + TTC_BC * NTG_CD - NTG_BC * NTG_CD),
                R = R1 * TTC_BC + R2 * TTC_CD
        )
    ;
        A = accel(D, Q) ->
        (   if
                ntg(B, D, NTG_BD, S), NTG_BD \= 0.0,
                ttc(B, D, TTC_BD, S), TTC_BD \= 0.0
            then
                (1.0 - Q) * TTC_BD / NTG_BD + Q \= 0.0,
                R = 1.0 / ((1.0 - Q) * TTC_BD / NTG_BD + Q) * TTC_BD
            else
                C = third_car_in_sight(B, D, S),
                ntg(B, C, NTG_BC, S), NTG_BC \= 0.0,
                ntg(C, D, NTG_CD1, S), NTG_CD1 \= 0.0,
                ttc(B, C, TTC_BC, S), TTC_BC \= 0.0,
                ttc(C, D, TTC_CD1, S), TTC_CD1 \= 0.0,
                (1.0 - Q) * TTC_CD1 / NTG_CD1 + Q \= 0.0,
                Q \= 0.0,
                NTG_CD = 1.0 / Q * NTG_CD1,
                TTC_CD = 1.0 / ((1.0 - Q) * TTC_CD1 / NTG_CD1 + Q) * TTC_CD1,
                R1 = (TTC_CD * NTG_BC) /
                     (NTG_CD * TTC_BC + TTC_CD * NTG_BC - NTG_CD * NTG_BC),
                R2 = (TTC_BC * NTG_CD - NTG_BC * NTG_CD) /
                     (NTG_BC * TTC_CD + TTC_BC * NTG_CD - NTG_BC * NTG_CD),
                R = R1 * TTC_BC + R2 * TTC_CD
        )
    ;
        A = sense(B, D, _, TTC_BD) ->
        R = TTC_BD
    ;
        A = sense(D, B, _, TTC_DB) ->
        R = TTC_DB
    ;
        A \= wait(_), A \= accel(B, _), A \= accel(D, _),
        ttc(B, D, R, S)
    ).

%-----------------------------------------------------------------------------%

lane(_, s0) = 0.
lane(B, do(A, S)) = L :-
    if      A = lc(B, L0)
    then    L = L0
    else    L = lane(B, S).

%-----------------------------------------------------------------------------%

start(s0) = 0.0.
start(do(A, S)) = T :-
    if      A = wait(D)
    then    T = start(S) + D
    else    T = start(S).

%-----------------------------------------------------------------------------%

poss(wait(T), wait(T), _) :- T > 0.0.

poss(accel(B, Q), accel(B, Q), _) :- Q >= 0.0.

poss(lc(B, L), lc(B, L), S) :- abs(lane(B, S) - L) = 1.

%-----------------------------------------------------------------------------%

%random_outcome(_, _, _) :- fail.

%-----------------------------------------------------------------------------%

%lookahead(_S) = 3.

%-----------------------------------------------------------------------------%

%:- func new_lookahead(lookahead, atom(prim, stoch)) = lookahead is det.

%new_lookahead(H, _C) = H - 1.

%-----------------------------------------------------------------------------%

%reward(s0) = 0.0.
%reward(do(_, S)) = reward(S).

%-----------------------------------------------------------------------------%

%proc(_, _) :- fail.

%-----------------------------------------------------------------------------%

/*
:- instance bat(prim, stoch, proc) where [
    pred(poss/3) is rstc.poss,
    pred(random_outcome/3) is rstc.random_outcome,
    func(reward/2) is rstc.reward,
    func(lookahead/1) is rstc.lookahead,
    func(new_lookahead/2) is rstc.new_lookahead,
    pred(proc/2) is rstc.proc
].
*/

/*
:- instance obs_bat(prim, stoch, proc, obs) where [
    pred(is_obs/1) is rstc.is_match_action,
    pred(covered_by_obs/1) is rstc.covered_by_match,
    func(obs_to_action/1) is rstc.obs2match
].

:- instance pr_bat(prim, stoch, proc, obs, env) where [
    seed_init_sit(I) = do(seed(I), s0),
    init_env_sit(env(T, Map), S) = do(init_env(env(T, Map)), S)
].
*/

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.
%-----------------------------------------------------------------------------%

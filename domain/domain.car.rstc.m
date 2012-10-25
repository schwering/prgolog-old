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

:- func ntg(car, car, sit) = ntg.
:- mode ntg(in, in, in) = out is semidet.

:- func ttc(car, car, sit) = ttc.
:- mode ttc(in, in, in) = out is semidet.

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

:- func float // float = float.
:- mode in // in = out is semidet.

V // W = R :- W \= 0.0, R = float.'/'(V, W).

%-----------------------------------------------------------------------------%

:- func min({float, car}::in, {float, car}::in) = ({float, car}::out) is det.

min(E1 @ {Cost1, _}, E2 @ {Cost2, _}) = ( if Cost1 < Cost2 then E1 else E2 ).


:- func third_car_in_sight(car::in, car::in, sit::in) = (car::out) is semidet.

third_car_in_sight(B, D, S) = E :-
    Cars = solutions((pred(C::out) is nondet :- car(C), C \= B, C \= D)),
    map((pred(C::in, {Cost, C}::out) is semidet :-
        ttc(B, C, S) \= 0.0,
        ttc(C, D, S) \= 0.0,
        Cost = abs(ntg(B, C, S) + ntg(C, D, S) + ttc(B, C, S) + ttc(C, D, S))
    ), Cars, [FirstCandidate | Candidates]),
    {_, E} = foldr(min, Candidates, FirstCandidate).

%-----------------------------------------------------------------------------%

ntg(B, D, s0) = (x(D) - x(B)) // v(B).
ntg(B, D, do(A, S)) = R :-
    B \= D,
    promise_equivalent_solutions [R]
    (
        A = wait(T),
        (   if
                ttc(B, D, S) \= 0.0
            then
                R = ntg(B, D, S) - T * ntg(B, D, S) // ttc(B, D, S)
            else
                C = third_car_in_sight(B, D, S),
                R1 = ntg(B, C, S) - T * ntg(B, C, S) // ttc(B, C, S),
                R2 = ttc(B, C, S) - T,
                R3 = ntg(C, D, S) - T * ntg(C, D, S) // ttc(C, D, S),
                R = R1 + (1.0 - R1 // R2) * R3
        )
    ;
        A = accel(B, Q),
        R = 1.0 // Q * ntg(B, D, S)
    ;
        A = sense(B, D, NTG_BD, _),
        R = NTG_BD
    ;
        A = sense(D, B, NTG_DB, TTC_DB),
        R = -1.0 // (1.0 - NTG_DB // TTC_DB) * NTG_DB
    /*  What to do when A = sense(_, _, _, _)?
     *  We could try to to do it transitively, but that probably wouldn't be
     *  intended if the preceding action was, e.g., sense(B, D, _, _).
     *  I guess this is related to the problem of deciding on a NTG/TTC in
     *  the presence of measuring inaccuracy. */
    ;
        A \= wait(_),
        A \= accel(B, _),
        A \= sense(B, D, _, _),
        A \= sense(D, B, _, _),
        R = ntg(B, D, S)
    ).

%-----------------------------------------------------------------------------%

ttc(B, D, s0) = (x(D) - x(B)) // (v(B) - v(D)).
ttc(B, D, do(A, S)) = R :-
    B \= D,
    promise_equivalent_solutions [R]
    (
        A = wait(T),
        R = ttc(B, D, S) - T
    ;
        A = accel(B, Q),
        (   if
                ntg(B, D, S) \= 0.0,
                ttc(B, D, S) \= 0.0
            then
                Q \= 1.0 - ntg(B, D, S) // ttc(B, D, S),
                R = 1.0 // ((Q - 1.0) * ttc(B, D, S) // ntg(B, D, S) + 1.0)
                    * ttc(B, D, S)
            else
                C = third_car_in_sight(B, D, S),
                Q \= 1.0 - ntg(B, C, S) // ttc(B, C, S),
                TTC_BC = 1.0 // ((Q - 1.0) * ttc(B, C, S) // ntg(B, C, S) + 1.0)
                    * ttc(B, C, S),
                R1 = (ttc(C, D, S) * ntg(B, C, S))
                    //  (ntg(C, D, S) * TTC_BC + ttc(C, D, S) * ntg(B, C, S)
                        - ntg(C, D, S) * ntg(B, C, S)),
                R2 = (TTC_BC * ntg(C, D, S) - ntg(B, C, S) * ntg(C, D, S))
                    //  (ntg(B, C, S) * ttc(C, D, S) + TTC_BC * ntg(C, D, S)
                        - ntg(B, C, S) * ntg(C, D, S)),
                R = R1 * TTC_BC + R2 * ttc(C, D, S)
        )
    ;
        A = accel(D, Q),
        (   if
                ntg(B, D, S) \= 0.0,
                ttc(B, D, S) \= 0.0
            then
                Q \= 1.0 // (1.0 - ntg(B, D, S) // ttc(B, D, S)),
                R = 1.0 // ((1.0 - Q) * ttc(B, D, S) // ntg(B, D, S) + Q)
                    * ttc(B, D, S)
            else
                C = third_car_in_sight(B, D, S),
                Q \= 1.0 // (1.0 - ntg(C, D, S) // ttc(C, D, S)),
                NTG_CD = 1.0 // Q * ntg(C, D, S),
                TTC_CD = 1.0 // ((1.0 - Q) * ttc(C, D, S) // ntg(C, D, S) + Q)
                    * ttc(C, D, S),
                R1 = (TTC_CD * ntg(B, C, S))
                    // (NTG_CD * ttc(B, C, S) + TTC_CD * ntg(B, C, S)
                        - NTG_CD * ntg(B, C, S)),
                R2 = (ttc(B, C, S) * NTG_CD - ntg(B, C, S) * NTG_CD)
                    // (ntg(B, C, S) * TTC_CD + ttc(B, C, S) * NTG_CD
                        - ntg(B, C, S) * NTG_CD),
                R = R1 * ttc(B, C, S) + R2 * TTC_CD
        )
    ;
        A = sense(B, D, _, TTC_BD),
        R = TTC_BD
    ;
        A = sense(D, B, _, TTC_DB),
        R = TTC_DB
    ;
        A \= wait(_),
        A \= accel(B, _),
        A \= accel(D, _),
        A \= sense(B, D, _, _),
        A \= sense(D, B, _, _),
        R = ttc(B, D, S)
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

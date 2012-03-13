% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0

:- module time.

:- interface.

:- use_module io.
:- pred main(io.io::di, io.io::uo) is det.


:- implementation.

:- import_module lp.
:- import_module prgolog.
:- import_module prgolog.fluents.
:- import_module prgolog.nice.
:- import_module int.
:- import_module float.
:- import_module list.
:- import_module math.
:- import_module maybe.
:- use_module random.
:- import_module string.
:- import_module solutions.
:- import_module term.
:- import_module table_statistics.

:- type velocity == float.
:- type yaw == float.

:- type prim --->   setVeloc(velocity, velocity, maybe(random.supply))
                ;   setYaw(yaw, yaw, maybe(random.supply)).
:- type stoch --->  setVelocSt(velocity)
                ;   setYawSt(yaw).
:- type procedure ---> drive.


:- func sitlen(sit(prim)) = int is det.

sitlen(s0) = 0.
sitlen(do(_, S)) = 1 + sitlen(S).


%:- type var_supply == var_supply(generic).

%:- pred collect(sit(prim)::in,
%                var_supply::in, var_supply::out,
%                equations::in,  equations::out) is det.

%collect(s0, !_VS, _, []).
%collect(do(A, S), !VS, !EQ).


:- pred poss(prim::in, prim::out, sit(prim)::in) is det.% semidet.

poss(A, A, _) :- A = setVeloc(_, _, _).
poss(A, A, _) :- A = setYaw(_, _, _).


:- pred random_supply(random.supply, sit(prim)).
:- mode random_supply(out, in) is det.

random_supply(RS, s0) :-
    random.init(3, RS).
random_supply(RS, do(A, S)) :-
    if      (   A = setVeloc(_, _, yes(RS0))
            ;   A = setYaw(_, _, yes(RS0)) )
    then    RS = RS0
    else    random_supply(RS, S).


:- pred random_normal(float::out, float::out,
                      random.supply::in, random.supply::out) is det.

random_normal(X1, X2, !RS) :-
    random.randmax(Max, !RS),
    random.random(R1, !RS),
    random.random(R2, !RS),
    U1 = float(R1) / float(Max) * 2.0 - 1.0,
    U2 = float(R2) / float(Max) * 2.0 - 1.0,
    Q = pow(U1, 2.0) + pow(U2, 2.0),
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

random_outcome(setVelocSt(V), setVeloc(V, Tol, yes(RS1)), S) :-
    random_supply(RS0, S),
    random_normal(Tol, _, RS0, RS1).
random_outcome(setYawSt(Y), setYaw(Y, Tol, yes(RS1)), S) :-
    random_supply(RS0, S),
    random_normal(_, Tol, RS0, RS1).


:- func reward(prog(prim, stoch, procedure), sit(prim)) = reward.
:- mode reward(unused, in) = out is det.

reward(_, S) = float(sitlen(S)).


:- func lookahead(sit(prim)) = lookahead is det.

lookahead(_S) = 2.


:- func new_lookahead(lookahead, atom(prim, stoch)) = lookahead is det.

new_lookahead(H, _C) = H - 1.


:- pred proc(procedure, prog(prim, stoch, procedure)).
:- mode proc(in, out) is det.

proc(drive, P) :- P = nil.


:- instance bat(time.prim, time.stoch, time.procedure) where [
    pred(poss/3) is time.poss,
    pred(random_outcome/3) is time.random_outcome,
    func(reward/2) is time.reward,
    func(lookahead/1) is time.lookahead,
    func(new_lookahead/2) is time.new_lookahead,
    pred(proc/2) is time.proc
].


% Solve the maze using a program:
%    (up | down | left | right)*
main(!IO) :-
    io.write("huhu", !IO), io.nl(!IO).


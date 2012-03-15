%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: time.m.
% Main author: schwering.
%
% Christoph Schwering (schwering@gmail.com)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module time.

:- interface.

:- use_module io.

%-----------------------------------------------------------------------------%

:- pred main(io.io::di, io.io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lp.
:- import_module prgolog.
:- import_module prgolog.ccfluents.
:- import_module prgolog.fluents.
:- import_module prgolog.nice.
:- import_module int.
:- import_module float.
:- import_module list.
:- import_module map.
:- import_module math.
:- import_module maybe.
:- use_module random.
:- import_module string.
:- import_module solutions.
:- import_module term.
:- import_module table_statistics.
:- use_module varset.

%-----------------------------------------------------------------------------%

:- type degree == float.
:- type radiant == float.
:- type kmh == float.
:- type ms == float.
:- type velocity == float.
:- type yaw == float.
:- type time == aterm.

:- type prim --->
        setVeloc(velocity, velocity, maybe(random.supply),
                 maybe(varset.varset), list(equation), time)
    ;   setYaw(yaw, yaw, maybe(random.supply), maybe(varset.varset),
               list(equation), time).
:- type stoch --->  setVelocSt(velocity)
                ;   setYawSt(yaw).
:- type procedure ---> drive.

%-----------------------------------------------------------------------------%

:- pred deg2rad(degree, radiant).
:- mode deg2rad(in, out) is det.
:- mode deg2rad(out, in) is det.
:- pragma promise_equivalent_clauses(deg2rad/2).

deg2rad(Deg::in, Rad::out) :- Rad = Deg / 180.0 * pi.
deg2rad(Deg::out, Rad::in) :- Deg = Rad * 180.0 / pi.


:- pred kmh2ms(kmh, ms).
:- mode kmh2ms(in, out) is det.
:- mode kmh2ms(out, in) is det.
:- pragma promise_equivalent_clauses(kmh2ms/2).

kmh2ms(Kmh::in, Ms::out) :- Ms = Kmh / 3.6.
kmh2ms(Kmh::out, Ms::in) :- Kmh = Ms * 3.6.


:- func deg_zero = (degree::out) is det.
:- func deg_min = (degree::out) is det.
:- func deg_max = (degree::out) is det.

deg_zero = 0.0.
deg_min = deg_zero - 25.0.
deg_max = deg_zero - 25.0.

%-----------------------------------------------------------------------------%

:- func sitlen(sit(prim)::in) = (int::out) is det.

sitlen(s0) = 0.
sitlen(do(_, S)) = 1 + sitlen(S).

%-----------------------------------------------------------------------------%

:- func notime = (time::out) is det.

notime = constant(-1.0).

%-----------------------------------------------------------------------------%

:- func start(sit(prim)::in) = (aterm::out) is det.

start(s0) = constant(0.0).
start(do(A, _)) = T :-
    (   A = setVeloc(_, _, _, _, _, T)
    ;   A = setYaw(_, _, _, _, _, T)
    ).

%-----------------------------------------------------------------------------%

:- func random_supply(sit(prim)::in) = (random.supply::out) is det.

random_supply(s0) = RS :-
    random.init(3, RS).
random_supply(do(A, S)) = RS :-
    if      (   A = setVeloc(_, _, yes(RS0), _, _, _)
            ;   A = setYaw(_, _, yes(RS0), _, _, _) )
    then    RS = RS0
    else    RS = random_supply(S).

%-----------------------------------------------------------------------------%

:- func varset(sit(prim)::in) = (varset.varset::out) is det.

varset(s0) = VS:-
    varset.init(VS).
varset(do(A, S)) = VS :-
    if      (   A = setVeloc(_, _, _, yes(VS0), _, _)
            ;   A = setYaw(_, _, _, yes(VS0), _, _) )
    then    VS = VS0
    else    VS = varset(S).

%-----------------------------------------------------------------------------%

:- func constraints(sit(prim)::in) = (list(equation)::out) is det.

constraints(s0) = [].
constraints(do(A, S)) = Cs ++ constraints(S) :-
    (   A = setVeloc(_, _, _, _, Cs, _)
    ;   A = setYaw(_, _, _, _, Cs, _) ).

%-----------------------------------------------------------------------------%

:- pred solve(sit(prim)::in) is semidet.

solve(S) :- solve(S, varset(S), []).


:- pred solve(sit(prim)::in, varset.varset::in, list(equation)::in) is semidet.

solve(S, VS, Cs) :- solve(S, VS, Cs, _, _).


:- pred solve(sit(prim)::in, varset.varset::in, list(equation)::in,
              float::out, map(var, float)::out) is semidet.

solve(S, VS, Cs, Val, Map) :-
    Constraints = Cs ++ constraints(S),
    Objective = variables(VS),
    lp_solve(Constraints, min, Objective, VS, [], R),
    R = satisfiable(Val, Map).

%-----------------------------------------------------------------------------%

:- pred poss(prim::in, prim::out, sit(prim)::in) is semidet.

poss(setVeloc(V, Tol, RS, no, [], notime),
     setVeloc(V, Tol, RS, yes(VS1), Cs, T), S) :-
    VS0 = varset(S),
    varset.new_var(TV, VS0, VS1),
    T = variable(TV),
    Cs = [T `>=` (start(S) + constant(1.0))],
    solve(S, VS1, Cs ++ constraints(S)).
poss(setYaw(Y, Tol, RS, no, [], notime),
     setYaw(Y, Tol, RS, yes(VS1), Cs, T), S) :-
    VS0 = varset(S),
    varset.new_var(TV, VS0, VS1),
    T = variable(TV),
    Cs = [T `>=` (start(S) + constant(1.0))],
    solve(S, VS1, Cs ++ constraints(S)).

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

random_outcome(setVelocSt(V), setVeloc(V, Tol, yes(RS1), no, [], notime), S) :-
    RS0 = random_supply(S),
    random_lognormal(1.0, 1.0, Tol, _, RS0, RS1).
random_outcome(setYawSt(Y), setYaw(Y, Tol, yes(RS1), no, [], notime), S) :-
    RS0 = random_supply(S),
    random_lognormal(1.0, 1.0, _, Tol, RS0, RS1).

%-----------------------------------------------------------------------------%

:- func reward(prog(prim, stoch, procedure), sit(prim)) = reward.
:- mode reward(unused, in) = out is det.

reward(_, S) = float(sitlen(S)).

%-----------------------------------------------------------------------------%

:- func lookahead(sit(prim)) = lookahead is det.

lookahead(_S) = 2.

%-----------------------------------------------------------------------------%

:- func new_lookahead(lookahead, atom(prim, stoch)) = lookahead is det.

new_lookahead(H, _C) = H - 1.

%-----------------------------------------------------------------------------%

:- pred proc(procedure, prog(prim, stoch, procedure)).
:- mode proc(in, out) is det.

proc(drive, P) :- P = nil.

%-----------------------------------------------------------------------------%

:- instance bat(time.prim, time.stoch, time.procedure) where [
    pred(poss/3) is time.poss,
    pred(random_outcome/3) is time.random_outcome,
    func(reward/2) is time.reward,
    func(lookahead/1) is time.lookahead,
    func(new_lookahead/2) is time.new_lookahead,
    pred(proc/2) is time.proc
].

%-----------------------------------------------------------------------------%

:- pred print(sit(prim)::in, io.io::di, io.io::uo) is det.

print(s0, !IO) :-
    io.nl(!IO).
print(do(A, S), !IO) :-
    print(S, !IO),
    io.write(A, !IO), io.nl(!IO).

%-----------------------------------------------------------------------------%

% Solve the maze using a program:
%    (up | down | left | right)*
main(!IO) :-
    io.write("huhu", !IO), io.nl(!IO),
    (   if      do(b(setYawSt(0.5)) `;` b(setVelocSt(15.0)), s0, S),
                solve(S, varset(S), [], Val, Map)
        then    print(S, !IO), io.nl(!IO),
                io.write(Val, !IO), io.nl(!IO),
                io.write(Map, !IO), io.nl(!IO),
                io.write(constraints(S), !IO), io.nl(!IO)
        else    io.write_string("failed", !IO), io.nl(!IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module time.
%-----------------------------------------------------------------------------%

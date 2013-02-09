%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012-2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.bat.m.
% Main author: schwering.
%
% The (rest of the) basic action theory based on RSTC.
% The action type and fundamental fluents are defined in the parent module.
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.bat.

:- interface.

:- use_module util.arithmetic.

%-----------------------------------------------------------------------------%

:- func rel_v(agent, agent, rstc.sit(N)) = scale(N) <= arithmetic.arithmetic(N).
:- mode rel_v(in, in, in) = out is semidet.

:- pred opposite_direction(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic.arithmetic(N).

:- pred same_direction(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic.arithmetic(N).

:- pred slower(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic.arithmetic(N).

:- pred faster(agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= arithmetic.arithmetic(N).

%-----------------------------------------------------------------------------%

:- func follow(agent, agent) `with_type` rstc.proc(N)
    <= arithmetic.arithmetic(N).

:- func tailgate(agent, agent) `with_type` rstc.proc(N)
    <= arithmetic.arithmetic(N).

:- func pass(agent, agent) `with_type` rstc.proc(N)
    <= arithmetic.arithmetic(N).

:- func overtake(agent, agent) `with_type` rstc.proc(N)
    <= arithmetic.arithmetic(N).

:- func approach(agent, agent) `with_type` rstc.proc(N)
    <= arithmetic.arithmetic(N).

:- func imitate(agent, agent) `with_type` rstc.proc(N)
    <= arithmetic.arithmetic(N).

%-----------------------------------------------------------------------------%

:- func sitlen(rstc.sit(_)) = int.

%-----------------------------------------------------------------------------%

:- instance bat(prim(N)) <= arithmetic.arithmetic(N).
:- instance obs_bat(prim(N), car_obs) <= arithmetic.arithmetic(N).
:- instance pr_bat(prim(N), car_obs) <= arithmetic.arithmetic(N).

%-----------------------------------------------------------------------------%

:- use_module io.

:- pred print_memo_stats(io.io::di, io.io::uo) is det.
:- pred reset_memo(io.io::di, io.io::uo) is det.

%-----------------------------------------------------------------------------%

:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module domain.car.rstc.fuzzy.
:- import_module prgolog.fluent.
:- import_module assoc_list.
:- import_module list.
:- import_module pair.
:- import_module util.pso.
:- import_module util.vector_space.
:- import_module util.vector_space.impl.

%-----------------------------------------------------------------------------%

:- pragma memo(reward/2, [allow_reset, statistics, fast_loose]). 

%-----------------------------------------------------------------------------%

rel_v(C, B, S) = one - ntg(B, C, S) / ttc(B, C, S).

opposite_direction(B, C, S) :- rel_v(B, C, S) > zero.

same_direction(B, C, S) :- rel_v(B, C, S) < zero.

slower(B, C, S) :- rel_v(B, C, S) > -one, rel_v(B, C, S) < one.

faster(B, C, S) :- not slower(B, C, S).

%-----------------------------------------------------------------------------%

:- pred actions_since(pred(A)::in(pred(in) is semidet),
                      prgolog.sit(A)::in, A::out) is nondet.

actions_since(P, do(A, S), A1) :-
    not P(A),
    (   actions_since(P, S, A1)
    ;   A = A1
    ).


:- func actions_since(pred(A)::in(pred(in) is semidet),
                      prgolog.sit(A)::in) = (list(A)::out) is det.

actions_since(P, S1) =
    ( if S1 = do(A, S), not P(A) then actions_since(P, S) ++ [A] else [] ).


:- pred to_be_copied(agent::in, agent::in, rstc.sit(N)::in,
                     rstc.prim(N)::out) is nondet.

to_be_copied(Copycat, Actor, S, A) :-
    BStart = (pred(start(B, _)::in) is semidet :- B = Copycat),
    actions_since(BStart, S, A),
    ( A = accel(Actor, _) ; A = lc(Actor, _) ).


:- func to_be_copied(agent, agent, rstc.sit(N)) = list(rstc.prim(N)).

to_be_copied(Copycat, Actor, S) = filter(P, actions_since(Q, S)) :-
    Q = (pred(start(B, _)::in) is semidet :- B = Copycat),
    P = (pred(A::in) is semidet :- ( A = accel(Actor, _) ; A = lc(Actor, _) )).


:- pred next_to_be_copied(agent::in, agent::in, rstc.sit(N)::in,
                          rstc.prim(N)::out) is semidet.

next_to_be_copied(Copycat, Actor, S, A1) :-
    % One thing I really don't like about Mercury: This one could be easily
    % implemented with actions_since/3, but then the determinism would be
    % cc_nondet.
    Q = (pred(start(B, _)::in) is semidet :- B = Copycat),
    P = (pred(A::in) is semidet :- ( A = accel(Actor, _) ; A = lc(Actor, _) )),
    find_first_match(P, actions_since(Q, S), A1).

%-----------------------------------------------------------------------------%

:- func accelf(agent, func(rstc.sit(N)) = num(N))
    `with_type` primf(rstc.prim(N)) <= arithmetic.arithmetic(N).
:- mode accelf(in, func(in) = out is det, in) = out is det.
:- mode accelf(in, func(in) = out is semidet, in) = out is det.

accelf(B, AccelF, S) =
    ( if Q = AccelF(S), not infinity(Q) then accel(B, Q) else abort ).


:- func lcf(agent) `with_type` primf(rstc.prim(N)) <= arithmetic.arithmetic(N).

lcf(B, S) = ( if lane(B, S) = left then lc(B, right) else lc(B, left) ).


:- func ntg_after(agent, agent, rstc.sit(N), s(N)) = ntg(N)
    <= arithmetic.arithmetic(N).
:- mode ntg_after(in, in, in, in) = out is semidet.

ntg_after(B, C, S, T) = ntg(B, C, prgolog.do(wait(T), S)).


:- func ttc_after(agent, agent, rstc.sit(N), s(N)) = ttc(N)
    <= arithmetic.arithmetic(N).
:- mode ttc_after(in, in, in, in) = out is semidet.

ttc_after(B, C, S, T) = ttc(B, C, prgolog.do(wait(T), S)).


:- func min_search_time = N <= arithmetic.arithmetic(N).

min_search_time = arithmetic.zero.


:- func max_search_time = N <= arithmetic.arithmetic(N).

max_search_time = arithmetic.from_int(60).


:- pred search(func(rstc.sit(N)) = N, N, N, rstc.sit(N))
    <= arithmetic.arithmetic(N).
:- mode search(in(func(in) = out is semidet), in, out, in) is semidet.
:- mode search(in(func(in) = out is det), in, out, in) is semidet.

search(F, Goal, X, S) :-
    arithmetic.lin_search(
        func(T) = F(prgolog.do(wait(number(T)), S)) is semidet,
        min_search_time, max_search_time, Goal, X).


:- func wait_until(func(rstc.sit(N)) = N, N)
    `with_type` primf(rstc.prim(N)) <= arithmetic.arithmetic(N).
:- mode wait_until(in(func(in) = out is semidet), in, in) = out is det.
:- mode wait_until(in(func(in) = out is det), in, in) = out is det.

wait_until(F, Goal, S) = A :-
    if   search(F, Goal, T, S)
    then A = wait(number(T))
    else A = abort.

%-----------------------------------------------------------------------------%

:- func basify(func(rstc.sit(N)) = num(N), rstc.sit(N)) = N.
:- mode basify(in(func(in) = out is semidet), in) = out is semidet.

basify(F, S) = X :- num(X) = F(S).


:- pragma foreign_decl("C", "static int counter = 0;").


:- pred inc_counter(int::out) is det.

:- pragma foreign_proc("C",
    inc_counter(I::out),
    [will_not_call_mercury, promise_pure],
"
    ++counter;
    I = counter;
").


:- pred dec_counter(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dec_counter(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure],
"
    --counter;
    IO1 = IO0;
").


:- func picknum({float, float}) `with_type` maxi_func(num(N))
    <= arithmetic.arithmetic(N).

picknum(Bounds, X0, Val, Cmp) = X1 :-
    NewVal = (func(Float) = Val(number_from_float(Float))),
    inc_counter(I), % XXX ugly hack!!!
    M = 1,
    N = 20 / (I*I),
    (   if      M > 0, N > 0
        then    run_pso(M, N, default_params, Bounds, max, NewVal, Cmp, X),
                X1 = number_from_float(X)
        else    X1 = X0
    ),
    trace [io(!IO)] ( dec_counter(!IO) ). % XXX ugly hack!!!


:- func pickaccel({float, float}, pickprog(A, num(N))) = prgolog.prog(A)
    <= arithmetic.arithmetic(N).

pickaccel(Bounds, Prog) = nice.pickbest(picknum(Bounds), one, Prog).


:- func picktuple({{float, float}, {float, float}}) `with_type` maxi_func({num(N), num(N)})
    <= arithmetic.arithmetic(N).

picktuple(Bounds, _X0, Val, Cmp) = {number_from_float(X), number_from_float(Y)} :-
    NewVal = (func({X1, Y1}) = Val({number_from_float(X1), number_from_float(Y1)})),
    run_pso(5, 10, default_params, Bounds, max, NewVal, Cmp, {X, Y}).


:- func pickacceltuple({{float, float}, {float, float}}, pickprog(A, {num(N), num(N)})) = prgolog.prog(A)
    <= arithmetic.arithmetic(N).

pickacceltuple(Bounds, Prog) = nice.pickbest(picktuple(Bounds), {one, one}, Prog).

%-----------------------------------------------------------------------------%

follow(B, Victim) =
    sync(
        a(start(B, $pred)) `;`
        t(r((pred(S::in) is semidet :-
            lane(B, S) = lane(Victim, S)
        ))) `;`
        t(r((pred(S::in) is semidet :-
            ntg(B, Victim, S) `in` close_behind
        ))) `;`
        pickaccel({0.0, 2.0}, func(X) = a(accel(B, X)))
        %b(accelf(B, rel_v(Victim, B)))
    ) `;`
    a(end(B, $pred)).


tailgate(B, Victim) =
    sync(
        a(start(B, $pred)) `;`
        t(r((pred(S::in) is semidet :-
            lane(B, S) = lane(Victim, S)
        ))) `;`
        t(r((pred(S::in) is semidet :-
            ntg(B, Victim, S) `in_any` [close_behind, very_close_behind]
        )))
    ) `;`
        pickaccel({0.0, 2.0}, func(X) = a(accel(B, X))) `;`
        %b(accelf(B, rel_v(Victim, B)))
    a(end(B, $pred)).


pass(B, C) =
    sync(
        a(start(B, $pred)) `;`
        t(r((pred(S::in) is semidet :-
            lane(B, S) \= lane(C, S),
            ntg(B, C, S) >= zero,
            ttc(B, C, S) > zero
        )))
    ) `;` (
%        t(r(pred(S::in) is semidet :-
%            ntg(B, C, S) `in_any` [very_close_infront, close_infront, infront, far_infront, very_far_infront]
%        ))
%        b(func(S) = (
%            if      ntg(B, C, S) `in_any` [very_close_infront, close_infront, infront, far_infront, very_far_infront]
%            then    noop
%            else if search(basify(ntg(B, C)), basic(defuzzify(very_close_infront)), T, S)
%            then    wait(number(T))
%            else    abort
%        ))
        star(pickaccel({0.95, 1.2}, func(X) = a(accel(B, X))))
    //
        a(abort)
    ) `;`
    sync(
        a(end(B, $pred)) `;`
%        nil
        t(r((pred(S::in) is semidet :-
            ntg(B, C, S) `in_any` [close_infront, very_close_infront]
        )))
    ).


overtake(B, C) =
    sync(
        a(start(B, $pred)) `;`
        t(r((pred(S::in) is semidet :-
            lane(B, S) = right,
            lane(C, S) = right,
            ntg(B, C, S) `in` close_behind
        )))
    ) `;` (
        (
            a(lc(B, left)) `;`
            t(r(pred(S::in) is semidet :-
                ntg(B, C, S) `in_any` [very_close_infront, close_infront, infront, far_infront, very_far_infront]
            )) `;`
%            b(func(S) = (
%                if      ntg(B, C, S) `in_any` [very_close_infront, close_infront, infront, far_infront, very_far_infront]
%                then    noop
%                else if search(basify(ntg(B, C)), basic(defuzzify(very_close_infront)), T, S)
%                then    wait(number(T))
%                else    abort
%            )) `;`
            a(lc(B, right))
        ) // (
            star(pickaccel({0.95, 1.2}, func(X) = a(accel(B, X))))
        )
    ) `;`
    sync(
        a(end(B, $pred)) `;`
%        nil
        t(r((pred(S::in) is semidet :-
            ntg(B, C, S) `in_any` [close_infront, very_close_infront]
        )))
    ).


approach(B, C) =
    sync(
        a(start(B, $pred)) `;`
        t(r((pred(S::in) is semidet :-
            ntg(B, C, S) > zero,
            ttc(B, C, S) > zero
        )))
    ) `;`
    star(pickaccel({0.95, 1.2}, func(X) = a(accel(B, X)))) `;`
    sync(
        a(end(B, $pred)) `;`
%        nil
        t(r((pred(S::in) is semidet :-
            ntg(B, C, S) `in_any` [close_behind, very_close_behind]
        )))
    ).


imitate(B, C) =
    a(start(B, $pred)) `;`
    star(
        b(func(S) = ( if next_to_be_copied(B, C, S, A) then A else abort ))
    ) `;`
    a(end(B, $pred)).

%-----------------------------------------------------------------------------%

:- pred poss(prim(N), rstc.sit(N)) <= arithmetic.arithmetic(N).
:- mode poss(in, in) is semidet.
:- mode poss(in(wait), in) is semidet.
:- mode poss(in(accel), in) is semidet.
:- mode poss(in(lc), in) is semidet.
:- mode poss(in(senseD), in) is det.
:- mode poss(in(senseL), in) is det.
:- mode poss(in(init_env), in) is det.
:- mode poss(in(seed), in) is det.
:- mode poss(in(abort), in) is failure.
:- mode poss(in(start), in) is det.
:- mode poss(in(end), in) is det.

poss(wait(T), _) :- T >= zero.
poss(accel(B, Q), S) :-
    no_duplicate(accel(B, Q), S).
%poss(accel(_, _Q), _).% :- Q >= zero.
poss(lc(B, L), S) :-
    ( lane(B, S) = left <=> L = right ),
    no_duplicate(lc(B, L), S).
%poss(lc(B, L), S) :- lane(B, S) = left <=> L = right.
%poss(lc(B, L), lc(B, L), S) :- abs(lane(B, S) - L) = 1.
poss(senseD(_, _, _, _), _).
poss(senseL(_, _), _).
poss(init_env(_), _).
poss(match(car_obs(Obs)), S) :- match_obs(Obs, S).
poss(seed(_), _).
poss(abort, _) :- fail.
poss(noop, _).
poss(start(_, _), _).
poss(end(_, _), _).


:- pred no_duplicate(prim(N), rstc.sit(N)) <= arithmetic.arithmetic(N).
:- mode no_duplicate(in(accel), in) is semidet.
:- mode no_duplicate(in(lc), in) is semidet.

no_duplicate(_, s0).
no_duplicate(A @ accel(B, _), do(A1, S1)) :-
        A1 = wait(_) %; A1 @< A, A1 \= accel(B, _), no_duplicate(A, S1).
    ;   ( A1 = accel(C, _) -> B @< C ; A1 \= accel(_, _) ), no_duplicate(A, S1).
no_duplicate(A @ lc(B, _), do(A1, S1)) :-
        A1 = wait(_) %; A1 @< A, A1 \= lc(B, _), no_duplicate(A, S1).
    ;   ( A1 = lc(C, _) -> B @< C ; A1 \= lc(_, _) ), no_duplicate(A, S1).

%-----------------------------------------------------------------------------%

:- func lookahead = lookahead is det.

lookahead = 3.


:- func lookahead(rstc.sit(_)) = lookahead is det.

lookahead(_S) = lookahead.


:- func new_lookahead(lookahead, rstc.sit(_)) = lookahead is det.

new_lookahead(L, S) =
    (   if      fail, S = do(start(_, _), _) then L
        else if fail, S = do(end(_, _), _) then L
        %else if S = do(match(_), _) then 0
        else    L - 1
    ).

%-----------------------------------------------------------------------------%

sitlen(s0) = 0.
sitlen(do(_, S)) = 1 + sitlen(S).
% XXX progression
%    (   if      A = init_env(rel_env(_, _, _, _, _, Sitlen))
%        then    Sitlen
%        else    1 + sitlen(S)
%    ).


    % Explanation of reward computation:
    % First of all, take the position of the interpreter. You compute the
    % sequence of actions A_1, ..., A_L for L being the lookahead which yields
    % the highest reward. Of this sequence, you execute A_1 and then repeat
    % the procedure.
    %
    % This allows for a trick: when an action's A* reward has reward sitlen(S)
    % (and all others have reward 0), the interpreter will defer A* as far as
    % possible. So the reward-maximizing sequence would be A_1, ..., A_L-1, A*.
    % In the next iteration, A* will again be deferred that way. Thus it won't
    % be exeucted before there's no alternative.
    %
    % Similarly, an action can be preferred: just subtract reward(S). To make
    % the action desirable at all, add some large positive constant which under
    % normal circumstances is ``guaranteed'' to be bigger than sitlen(S).
    %
    % Okay, now why should we want to prefer start(_, _)?
    % Consider a scenario where all cars simply maintain their respective speed.
    % Therefore no actions are needed to explain all incoming observations.
    % However, if the hypothesis is exactly that -- just do nothing -- you want
    % this hypothesis to occur in the situation term. In fact, it should occur
    % right before there is indeed no further change needed. This is enforced by
    % preferring start(_, _) using a reward like N - sitlen(S) for some large
    % positive constant N.
    % The start(_, _) action, by the way, also has another use. Since it must
    % occur somewhere in the hypothesis program, the program may couple it with
    % some tests. These tests might assert a certain order of vehicles, for
    % example. By making the start(_, _) and the tests a complex action, the
    % interpreter is forced to execute them together.
    %
    % And why do we want to defer the end(_, _) action?
    % The occurence of end(_, _) in a situation term should indicate that the
    % hypothesis explained the world up to this point. Again consider the
    % scenario where each car maintains its speed, and the hypothesis program
    % is exactly that. Now you could execute end(_, _) right after start(_, _),
    % but that's not very informative given that the cars might have maintained
    % their speeds for the whole next minute. So you want end(_, _) not to be
    % executed before its inevitable, that is, the hypothesis program doesn't
    % explain the world anymore.
    %
    % So much for the general idea and motivation. In this BAT, an observation
    % consists of a pair of actions: wait(_) and match(_). The reward of
    % wait(_) is 0 as it's a general-purpose action; the reward of match(_) is
    % 1.0 + (1.0 - match_dist) where match_dist is measures how ``badly'' the
    % the observation is entailed. match_dist is a number between 0 and 1.
    % Therefore, the reward of any match(_) action is between 1.0 and 2.0.
    % As a match(_) action never occurs alone but always with an accompanying
    % wait(_), the reward for any observation ``per action'' is between 0.0 and
    % 1.0.
    % With a lookahead of L, the interpreter can thus generate at most a reward
    % of L only due to observations.
    % Therefore sitlen(S) is an upper bound of the reward gained in during the
    % lookahead only due to observations.
    %
    % To prefer the start(_, _) action we therefore set its reward to
    % N - 3.0 * sitlen(S). That is, the punishment of delaying start(_, _) a
    % single step is 2.0, which is greater than what could be gained by
    % executing an observation, which is 1.0. (Strictly speaking the observation
    % would gain a reward 2.0, but distributed over two actions, so the
    % start(_, _) action would have to be dealyed by two steps which would give
    % a punishment of 4.0.)
    %
    % To defer the end(_, _) action we set the reward to 2.0 * sitlen(S).
    % Thus the interpreter is awarded for each step it delays end(_, _) by 2.0,
    % whereas any observation would give him only 1.0.
    %
    % There's one caveat with this theory with the end(_, _) action: the
    % interpreter might see the end(_, _) action at the end of its lookahead and
    % be tempted by its sweet reward. Thus it decides to simply execute the
    % candidate program quickly just to as quickly get to the end(_, _) action.
    % We avoid this effect with the hack that we return 2.0 * sitlen(S) only
    % if the last executed action was a match(_) action. For this means that
    % the whole candidate program except for the end(_, _) action is covered
    % by match(_) actions. Thus if it can't execute the next match(_) action,
    % the interpreter will go for the end(_, _) action, whereas before it
    % executed the next observation's wait(_) and then failed at match(_).
    % On the other hand, if it can execute the next match(_) action, it will
    % do so for the reasons above. Therefore:
    % It will opt for end(_, _) iff it cannot explain the next observation.
    %
    % Note that these tricks do not work as intended if lookahead L = 1.
    % In fact, it even earlier breaks: if you have an action A with some effect,
    % an observation consisting of two actions W and M which is entailed by A's
    % effect, and an end(_, _) action E, the indended order to compute the
    % reward should be A; W; M; E because this gives us the reward of the
    % observation and of a as-late-as-possible E.
    % However, for a reward less than 4 the interpreter will do things like A; E
    % because the end(_, _) action E overrules the observation W and M.
    %
:- func reward(rstc.prim(N), rstc.sit(N)) = reward <= arithmetic.arithmetic(N).
:- mutable(reward_counter, int, 0, ground, [attach_to_io_state, untrailed]).

reward(A, S) = New :-
    trace [io(!IO)] ( get_reward_counter(I, !IO), set_reward_counter(I+1, !IO) ),
    New = (
% XXX progression
%        if      some [Reward] A = init_env(rel_env(_, _, _, _, Reward, _))
%        then    Reward
        if A = start(_, _)
        then    float(max(0, 1000 - 2 * sitlen(S)))
        else if A = end(_, _)
        then    ( S = do(match(_), _) -> float(2 * sitlen(S)) ; 0.0 )
        %then    0.0% -1.0% float(2 * sitlen(S))
        else if A = match(car_obs(Obs))
        %then    1.0 + (1.0 - arithmetic.to_float(det_basic(match_dist(Obs, S))))
        % About two times faster than match_dist/2:
        then    1.0 + (1.0 - 0.0)
        else if A = accel(_, _) ; A = lc(_, _)
        then    -0.01
        else    0.0
    ).


:- func reward_bound(atom(rstc.prim(_))) = reward.
:- mutable(reward_bound_counter, int, 0, ground, [attach_to_io_state, untrailed]).

reward_bound(A) = R :-
    trace [io(!IO)] ( get_reward_bound_counter(I, !IO), set_reward_bound_counter(I+1, !IO) ),
    R = (
% XXX progression
%        if      some [Reward] A = prim(init_env(rel_env(_, _, _, _, Reward, _)))
%        then    Reward
        if A = prim(start(_, _))
        then    1000.0
        else if A = prim(end(_, _))
        then    1000.0
        else if A = prim(match(_))
        then    2.0
        else    0.0
    ).


%-----------------------------------------------------------------------------%

:- pred match_obs(Obs::in, rstc.sit(N)::in) is semidet
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

match_obs(Obs, S) :-
    all_true((pred({B, C}::in) is semidet :-
        match_info(Obs, B, C, S)
    ), agent_pairs),
    all_true((pred(B::in) is semidet :-
        match_y(Obs, B, S)
    ), agents).


:- func match_dist(Obs, rstc.sit(N)) = num(N)
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

match_dist(Obs, S) =
    (   if   Len = 0
        then zero
        else number(Sum `arithmetic.'/'` arithmetic.from_int(Len))
    ) :-
    %Zero = {arithmetic.zero, 0},
    %Plus = (func({U, V}, {X, Y}) = {U `arithmetic.'+'` X, V `int.'+'` Y}),
    Sum = foldl((func({B, C}, Sum0) = Sum0 `arithmetic.'+'`
        match_longitudinal_dist(Obs, B, C, S)
    ), agent_pairs, arithmetic.zero),
    Len = length(agent_pairs).


:- pred ntgs(Obs::in, agent::in, agent::in, rstc.sit(N)::in,
             num(N)::out, num(N)::out) is semidet
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

ntgs(Obs, B, C, S, ntg(B, C, S), number_from_float(net_time_gap(Obs, B, C))).


:- pred ttcs(Obs::in, agent::in, agent::in, rstc.sit(N)::in,
             num(N)::out, num(N)::out) is semidet
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

ttcs(Obs, B, C, S, ttc(B, C, S), number_from_float(time_to_collision(Obs, B, C))).


:- pred have_common_cat(pred(category), num(N), num(N))
    <= arithmetic.arithmetic(N).
:- mode have_common_cat(pred(out) is multi, in, in) is semidet.

have_common_cat(P, V1, V2) :-
    P(Cat),
    V1 `in` Cat,
    V2 `in` Cat.

    % When two cars have the same speed, TTC is +/-inf.
    % In practice, though, they never have the same speed but continuously
    % balance their speeds. That is, sometimes v(B) >= v(C) and sometimes
    % v(B) =< v(C).
    % This leads to oscillation of TTC: sometimes its +X, then again -X
    % for some really big number X.
    %
    % Problem: our internal model doesn't reflect these oscillations.
    % In fact, it's a weakness of the TTC metric, I guess.
    %
    % To deal with this effect, we allow the model's TTC and the observed
    % TTC to fall into different categories, as long as (= if!) the
    % relative velocity delta does not exceed the value defined by this
    % function:
    %
:- func max_veloc_discrepancy_to_ignore_ttc = float.

max_veloc_discrepancy_to_ignore_ttc = 0.1.


:- pred match_info(Obs::in, agent::in, agent::in, rstc.sit(N)::in) is semidet
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

match_info(Obs, B, C, S) :-
    (   if      some [NTG1, NTG2] ntgs(Obs, B, C, S, NTG1, NTG2)
        then    have_common_cat((pred(Cat::out) is multi :-
                    ntg_cat(Cat)
                ), NTG1, NTG2)
        else    true
    ),
    (   if      some [TTC1, TTC2] ttcs(Obs, B, C, S, TTC1, TTC2)
        then    (   have_common_cat((pred(Cat::out) is multi :-
                        ttc_cat(Cat)
                    ), TTC1, TTC2)
                ;   some [NTG1, NTG2] (
                        ntgs(Obs, B, C, S, NTG1, NTG2),
                        RelV1 = one - NTG1 / TTC1,
                        RelV2 = one - NTG2 / TTC2,
                        Eps = max_veloc_discrepancy_to_ignore_ttc,
                        abs(RelV1 - one) =< number_from_float(Eps),
                        abs(RelV2 - one) =< number_from_float(Eps)
                    )
                )
        else    true
    ).


:- func match_longitudinal_dist(Obs, agent, agent, rstc.sit(N)) = N
    <= (arithmetic.arithmetic(N), car_obs(Obs)).

match_longitudinal_dist(Obs, B, C, S) = D :-
    D1 = (  if      some [NTG1, NTG2] ntgs(Obs, B, C, S, NTG1, NTG2)
            then    minimize(pred(CatDist::out) is multi :-
                        (
                            CatDist = one
                        ;
                            ntg_cat(Cat),
                            NTG1 `in` Cat,
                            NTG2 `in` Cat,
                            CatDist = abs(NTG1 - NTG2) / max_width(Cat)
                        )
                    )
            else    zero
        ),
    D2 = (  if      some [TTC1, TTC2] ttcs(Obs, B, C, S, TTC1, TTC2)
            then    minimize(pred(CatDist::out) is multi :-
                        (
                            CatDist = one
                        ;
                            ttc_cat(Cat),
                            TTC1 `in` Cat,
                            TTC2 `in` Cat,
                            (
                                CatDist = abs(TTC1 - TTC2) / max_width(Cat)
                            ;
                                % If they have almost same speed, don't punish
                                % this. See max_veloc_discrepancy_to_ignore_ttc.
                                some [NTG1, NTG2] (
                                    ntgs(Obs, B, C, S, NTG1, NTG2),
                                    RelV1 = one - NTG1 / TTC1,
                                    RelV2 = one - NTG2 / TTC2,
                                    Eps = max_veloc_discrepancy_to_ignore_ttc,
                                    abs(RelV1 - one) =< number_from_float(Eps),
                                    abs(RelV2 - one) =< number_from_float(Eps)
                                ),
                                CatDist = zero
                            )
                        )
                    )
            else    zero
        ),
    (   if   D3 = (D1 + D2) / (one + one), basic(D3)
        then D = det_basic(D3)
        else unexpected($module, string.format("%s: sum not defined: %s, %s",
                                               [s($pred), s(string(D1)),
                                                s(string(D2))]))
    ).


:- pred match_y(Obs::in, agent::in, rstc.sit(_)::in) is semidet <= car_obs(Obs).

match_y(Obs, B, S) :-
    if      Y = y_pos(Obs, B)
    then    ( Y =< 0.0, lane(B, S) = right
            ; Y >= 0.0, lane(B, S) = left )
    else    true.

%-----------------------------------------------------------------------------%

:- pred is_obs_prog(pseudo_atom(prim(N))::in) is semidet
    <= arithmetic.arithmetic(N).

is_obs_prog(complex(seq(pseudo_atom(atom(primf(_))),
                        pseudo_atom(atom(prim(match(_))))))).
is_obs_prog(atom(prim(match(_)))).

%-----------------------------------------------------------------------------%

:- func obs_to_action(car_obs) = pseudo_atom(prim(N))
    <= arithmetic.arithmetic(N).

obs_to_action(CarObs @ car_obs(Obs)) =
    complex(seq(pseudo_atom(atom(Wait)), pseudo_atom(atom(Match)))) :-
    T1 = number_from_float(time(Obs)),
    Wait = primf(
        func(S) = (
            if T0 = start(S), T0 = num(_) then wait(T1 - T0) else abort
        )
    ),
    Match = prim(match(CarObs)).

%-----------------------------------------------------------------------------%

:- instance bat(prim(N)) <= arithmetic.arithmetic(N) where [
    pred(poss/2) is bat.poss,
    func(reward_bound/1) is bat.reward_bound,
    func(reward/2) is bat.reward,
    func(lookahead/1) is bat.lookahead
].

:- instance obs_bat(prim(N), car_obs) <= arithmetic.arithmetic(N) where [
    (is_obs_action(match(_))),
    (covered_by_obs(do(A, S)) :-
        (   A \= wait(_), covered_by_obs(S)
        ;   A = match(_) )
    ),
    pred(is_obs_prog/1) is bat.is_obs_prog,
    func(obs_to_action/1) is bat.obs_to_action
].

:- instance pr_bat(prim(N), car_obs) <= arithmetic.arithmetic(N) where [
    seed_init_sit(I) = do(seed(I), s0),
    init_env_sit(Obs, S) = do(init_env(Obs), S)
].

%-----------------------------------------------------------------------------%

:- import_module table_statistics.

print_memo_stats(!IO) :-
    table_statistics_for_reward_2(Lane, !IO),
    io.write_string("\nreward/2:\n", !IO),
    write_table_stats(current_stats(call_table_stats(Lane)), !IO),
    true.


reset_memo(!IO) :-
    table_reset_for_reward_2(!IO),
    get_reward_counter(I, !IO),
    format("REWARD COUNTER = %d\n", [i(I)], !IO),
    get_reward_bound_counter(J, !IO),
    format("REWARD BOUND COUNTER = %d\n", [i(J)], !IO),
    true.

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.bat.
%-----------------------------------------------------------------------------%

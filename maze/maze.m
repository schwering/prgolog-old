% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Simple maze 6 x 6 with four 3 x 3 rooms at the top left, top right, bottom
% left, and bottom right. The border between two rooms consists of 3 fields,
% the center of which is a door.
%
% It looks like this:
%
%   S++ +++
%   +++-+++
%   +++ +++
%    |   |
%   +++ +++
%   +++-+++
%   +++ ++G
%
% The agent starts at S and wants to get to G. You may change these by modifying
% the start/0 and goal/0 functions.
% It may move to any field marked with a +, and may only move up, down, left,
% or right. The fields labeled with | and - are no actual fields the robot can
% stand on, but represent doors between rooms, that is, they connect two +
% signs.
%
% As reward function, we use the `inverse' Manhattan distance or Euclidean
% distance (adjust the dist/2 function appropriately).
%
% The fluents, pos and unvisited, are not implemented as successor state axioms
% but in a more Prolog-way, which should be way more efficient.
% E.g., we don't have to regress the stupid position for each predecessor
% situation in unvisited, but continuously compare the input position with the
% predecessor situation's positions).
% I don't know whether or not this is okay. Maybe we could choose our fluents so
% that even the SSAs are efficient?
%
%
% After getting the Mercury compiler from [1] and installing it (compilation
% takes much time, but it's worth it), you can compile this example if maze.m
% and prgolog.m are in the current directory with
%
% $ mmc --make maze
%
% and then you can simply run
%
% $ ./maze
%
% [1] http://www.mercury.csse.unimelb.edu.au/
%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)

:- module maze.

:- interface.

:- use_module io.
:- pred main(io.io::di, io.io::uo) is det.


:- implementation.

:- import_module prgolog.
:- import_module prgolog.fluent.
:- import_module prgolog.nice.
:- import_module int.
:- import_module float.
:- use_module hash_table.
:- import_module list.
:- import_module math.
:- use_module random.
:- use_module string.
:- import_module solutions.
:- import_module table_statistics.

:- type point ---> p(int, int).


:- func room_size   = int is det.
:- func room_height = int is det.
:- func room_width  = int is det.

room_size = 80.
room_height = room_size.
room_width = room_height.


:- func start = point is det.
:- func goal  = point is det.

start = p(0, 0).
%goal = p(4, 4).
goal = p(2 * room_width - 1, 2 * room_height - 1).


:- pred in_maze(point::in) is semidet.

in_maze(p(X, Y)) :-
    0 =< X, X < 2 * room_width,
    0 =< Y, Y < 2 * room_height.


:- pred at_upper_wall(int::in) is semidet.
:- pred at_lower_wall(int::in) is semidet.
:- pred at_left_wall(int::in)  is semidet.
:- pred at_right_wall(int::in) is semidet.

at_upper_wall(Y) :-  Y      mod room_height = 0.
at_lower_wall(Y) :- (Y + 1) mod room_height = 0.
at_left_wall(X)  :-  X      mod room_width  = 0.
at_right_wall(X) :- (X + 1) mod room_width  = 0.


:- pred at_td_door(int::in) is semidet.
:- pred at_lr_door(int::in) is semidet.

at_td_door(X) :- X mod room_width = room_width / 2.
at_lr_door(Y) :- Y mod room_height = room_height / 2.


:- func north(point) = point is det.
:- func south(point) = point is det.
:- func west(point) = point is det.
:- func east(point) = point is det.

north(p(X, Y)) = p(X, Y-1).
south(p(X, Y)) = p(X, Y+1).
west(p(X, Y))  = p(X-1, Y).
east(p(X, Y))  = p(X+1, Y).


:- pred conn(point, point).
:- mode conn(in, out) is nondet.
:- mode conn(in, in) is semidet.

conn(P1, P2) :-
    P1 = p(X, Y),
    in_maze(P1),
    in_maze(P2),
    (   P2 = north(P1), ( if not at_upper_wall(Y) then true else at_td_door(X) )
    ;   P2 = south(P1), ( if not at_lower_wall(Y) then true else at_td_door(X) )
    ;   P2 = west(P1),  ( if not at_left_wall(X)  then true else at_lr_door(Y) )
    ;   P2 = east(P1),  ( if not at_right_wall(X) then true else at_lr_door(Y) )
    ).


:- func dist(point, point) = float is det.

dist(p(X1, Y1), p(X2, Y2)) =
    math.sqrt(pow(float(X1-X2), 2) + pow(float(Y1-Y2), 2)).
    %floor_to_int(math.sqrt(pow(float(X1-X2), 2) + pow(float(Y1-Y2), 2))).
%dist(p(X1, Y1), p(X2, Y2)) = abs(X1 - X2) + abs(Y1 - Y2).


:- type prim ---> left ; right ; up ; down
                ; left_out(stoch, int, random.supply)
                ; right_out(stoch, int, random.supply)
                ; up_out(stoch, int, random.supply)
                ; down_out(stoch, int, random.supply).
:- type stoch ---> s_left ; s_right ; s_up ; s_down.
:- type procedure ---> bla.


:- func sitlen(sit(prim)) = int is det.

sitlen(s0)       = 0.
sitlen(do(_, S)) = 1 + sitlen(S).


:- pred poss(prim::in, prim::out, sit(prim)::in) is semidet.

poss(A, A, S) :-
    P1 = pos(S),
    P2 = new_pos(A, P1),
    conn(P1, P2),
    unvisited(P2, S).


:- pred random_supply(random.supply, sit(prim)).
:- mode random_supply(out, in) is det.

random_supply(RS, s0) :-
    random.init(3, RS).
random_supply(RS, do(A, S)) :-
    if      (   A = up_out(_, _, RS0)
            ;   A = down_out(_, _, RS0)
            ;   A = left_out(_, _, RS0)
            ;   A = right_out(_, _, RS0) )
    then    RS = RS0
    else    random_supply(RS, S).


:- pred random_outcome(stoch, prim, sit(prim)).
:- mode random_outcome(in, out, in) is det.

random_outcome(B, A, S) :-
    random_supply(RS0, S),
    random.random(R, RS0, _),
    (   B = s_up, Y = 7
    ;   B = s_down, Y = 13
    ;   B = s_left, Y = 19
    ;   B = s_right, Y = 31
    ),
    X = R mod 100,
    random.init(R * Y, RS1),
    Aup    = up_out(B, X, RS1),
    Adown  = down_out(B, X, RS1),
    Aleft  = left_out(B, X, RS1),
    Aright = right_out(B, X, RS1),
    (   B = s_up,    {A0, A1, A2, A3} = {Adown, Aleft, Aright, Aup}% {Adown,  Aleft,  Aright, Aup}
    ;   B = s_down,  {A0, A1, A2, A3} = {Aup, Aleft, Aright, Adown}% {Aup,    Aright, Aleft,  Adown}
    ;   B = s_left,  {A0, A1, A2, A3} = {Aup, Adown, Aright, Aleft}% {Aright, Aup,    Adown,  Aleft}
    ;   B = s_right, {A0, A1, A2, A3} = {Aup, Adown, Aleft, Aright}% {Aleft,  Adown,  Aup,    Aright}
    ),
    (   if       0 =< X, X < 10, maze.poss(A0, _, S) then A = A0
        else if 10 =< X, X < 30, maze.poss(A1, _, S) then A = A1
        else if 30 =< X, X < 50, maze.poss(A2, _, S) then A = A2
        else                                           A = A3
    ),
    %trace [io(!IO)] (
        %io.write(A, !IO), io.nl(!IO)
    %).
    true.


:- func reward(prog(prim, stoch, procedure), sit(prim)) = reward.
:- mode reward(unused, in) = out is det.

reward(_, S) = (dist(start, goal) - dist(pos(S), goal))
             * (dist(start, goal) - dist(pos(S), goal))
             - float(sitlen(S)).


:- func lookahead(sit(prim)) = lookahead is det.

lookahead(_S) = 5.


:- func new_lookahead(lookahead, atom(prim, stoch)) = lookahead is det.

new_lookahead(H, _C) = H - 1.


:- pred proc(procedure, prog(prim, stoch, procedure)).
:- mode proc(in, out) is det.

proc(P, P1) :- P = bla, P1 = nil.


:- instance bat(maze.prim, maze.stoch, maze.procedure) where [
    pred(poss/3) is maze.poss,
    pred(random_outcome/3) is maze.random_outcome,
    func(reward/2) is maze.reward,
    func(lookahead/1) is maze.lookahead,
    func(new_lookahead/2) is maze.new_lookahead,
    pred(proc/2) is maze.proc
].


:- func new_pos(prim, point) = point is det.

new_pos(A, P1) = P2 :-
    (   A = up,             P2 = north(P1)
    ;   A = down,           P2 = south(P1)
    ;   A = left,           P2 = west(P1)
    ;   A = right,          P2 = east(P1)
    ;   A = up_out(_, _, _),   P2 = north(P1)
    ;   A = down_out(_, _, _), P2 = south(P1)
    ;   A = left_out(_, _, _), P2 = west(P1)
    ;   A = right_out(_, _, _),P2 = east(P1)
    ).


:- func pos(sit(prim)) = point is det.
%:- pragma memo(pos/1, [allow_reset, fast_loose]). 

pos(S1) = P :-
    (   S1 = s0, P = start
    ;   S1 = do(A, S), P = new_pos(A, pos(S))
    ).


:- pred unvisited(point::in, sit(prim)::in) is semidet.

unvisited(P, S1) :- standalone_unvisited(P, S1).


:- pred naive_unvisited(point::in, sit(prim)::in) is semidet.

naive_unvisited(P, S1) :-
    pos(S1) \= P,
    (   S1 = do(_, S), naive_unvisited(P, S)
    ;   S1 = s0
    ).


:- pred standalone_unvisited(point::in, sit(prim)::in) is semidet.

standalone_unvisited(P, S) :- standalone_unvisited(P, _, S).


:- pred standalone_unvisited(point::in, point::out, sit(prim)::in) is semidet.

standalone_unvisited(P1, P3, S1) :-
    (   S1 = s0, P3 = start
    ;   S1 = do(A, S), standalone_unvisited(P1, P2, S), P3 = new_pos(A, P2)
    ),
    P1 \= P3.


/*
:- use_module term.
:- use_module univ.
:- pred test(io.io::di, io.io::uo) is det.
test(!IO) :-
    term.init_var_supply(VS),
    term.create_var(Var, VS, _),
    Vals = [up, down, left, right],
    Ctx = term.context($file, $line),
    term.det_term_to_type(term.variable(Var, Ctx), VarTerm),
    Prog = pseudo_atom(atom(prim(up))),
    NewProg = pick2(Var, Vals, Prog),
    io.write(NewProg, !IO), io.nl(!IO).
*/


% Solve the maze using a program:
%    (up | down | left | right)*
main(!IO) :-
    %(   if      X = mycons(Y), instantiate(Y, [c1, c2]), Y \= c1
    %    then    io.write_string("ok", !IO), io.nl(!IO),
    %            io.write(X, !IO), io.nl(!IO)
    %    else    io.write_string("no", !IO), io.nl(!IO)
    %),
    %test(!IO),
    RewMax = dist(start, goal),
    io.write(RewMax, !IO), io.nl(!IO),
    Pos0 = pos(s0),
    io.write(Pos0, !IO), io.nl(!IO),
    io.write(start, !IO), io.nl(!IO),
    io.write(goal, !IO), io.nl(!IO),
    Prog = star(b(s_up) or b(s_down) or b(s_left) or b(s_right)),
    %Prog1 = t(pos == f(start)) `;` Prog0 `;` t(pos == f(goal)),
    (   if      do(Prog, s0, S1)
        then    Rew1 = maze.reward(_, S1),
                Pos1 = pos(S1),
                %( if Rew1 = RewMax then Msg = "ok" else Msg = "failed early" ),
                ( if pos(S1) = goal then Msg = "ok" else Msg = "failed early" ),
                io.format("%s\n", [string.s(Msg)], !IO),
                io.write(Rew1, !IO), io.nl(!IO),
                io.write(Pos1, !IO), io.nl(!IO),
                io.write(S1, !IO), io.nl(!IO)
        else    io.format("fail\n", [], !IO)
    ),
    %table_statistics_for_pos_1(StatsPos, !IO),
    %table_statistics_for_unvisited_2(StatsUnv, !IO),
    %io.format("\n\npos/1\n", [], !IO),
    %write_table_stats(current_stats(call_table_stats(StatsPos)), !IO),
    %io.format("\n\nunvisisted/2\n", [], !IO),
    %write_table_stats(current_stats(call_table_stats(StatsUnv)), !IO),
    true.


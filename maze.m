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
%   +++ ++G
%   +++-+++
%   +++ +++
%
% The agent starts at S and wants to get to G. You may change these by modifying
% the start/0 and goal/0 functions.
% It may move to any field marked with a +, and may only move up, down, left,
% or right.
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

:- module maze.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.
:- pred main1(io::di, io::uo) is det.
:- pred main2(io::di, io::uo) is det.


:- implementation.

:- import_module prgolog.
:- import_module int.
:- import_module float.
:- import_module list.
:- import_module math.
:- import_module string.

:- type point ---> p(int, int).

:- pred in_maze(point).
:- mode in_maze(in) is semidet.
in_maze(p(X, Y)) :-
    0 =< X, X < 6,
    0 =< Y, Y < 6.

:- pred at_wall(int::in) is semidet.
at_wall(I) :-
    (   I mod 3 = 0
    ;   I mod 3 = 2
    ).

:- pred at_door(int::in) is semidet.
at_door(I) :-
    I mod 3 = 1.

:- func north(point) = point is det.
north(p(X, Y)) = p(X, Y-1).

:- func south(point) = point is det.
south(p(X, Y)) = p(X, Y+1).

:- func west(point) = point is det.
west(p(X, Y)) = p(X-1, Y).

:- func east(point) = point is det.
east(p(X, Y)) = p(X+1, Y).

:- pred conn(point, point).
:- mode conn(in, out) is nondet.
:- mode conn(in, in) is semidet.
conn(P1, P2) :-
    P1 = p(X1, Y1),
    in_maze(P1),
    in_maze(P2),
    (   P2 = north(P1), ( if Y1 mod 3 \= 0 then true else at_door(X1) )
    ;   P2 = south(P1), ( if Y1 mod 3 \= 2 then true else at_door(X1) )
    ;   P2 = west(P1),  ( if X1 mod 3 \= 0 then true else at_door(Y1) )
    ;   P2 = east(P1),  ( if X1 mod 3 \= 2 then true else at_door(Y1) )
    ).

:- func start = point is det.
start = p(0, 0).

:- func goal = point is det.
goal = p(5, 3).

:- func dist(point, point) = int is det.
dist(p(X1, Y1), p(X2, Y2)) = floor_to_int(math.sqrt(pow(float(X1-X2), 2) + pow(float(Y1-Y2), 2))).
%dist(p(X1, Y1), p(X2, Y2)) = abs(X1 - X2) + abs(Y1 - Y2).


:- type prim_action ---> left ; right ; up ; down.
:- type stoch_action ---> s_left ; s_right ; s_up ; s_down.
:- type procedure ---> bla.


:- pred poss(prim_action::in, sit(prim_action)::in) is semidet.
poss(A, S) :-
    P1 = pos(S),
    P2 = new_pos(A, P1),
    conn(P1, P2),
    unvisited(P2, S).


:- pred random_outcome(stoch_action::in, prim_action::out, S::in) is det.
random_outcome(B, A, _S) :-
    (   B = s_up,    A = up
    ;   B = s_down,  A = down
    ;   B = s_left,  A = left
    ;   B = s_right, A = right
    ).


:- func reward(sit(prim_action)) = int is det.
reward(S) = dist(start, goal) - dist(pos(S), goal).


:- func horizon(sit(prim_action)) = horizon is det.
horizon(_S) = 5.


:- func new_horizon(horizon, atom(prim_action, stoch_action)) = horizon is det.
new_horizon(H, _C) = H - 1.


:- pred proc(procedure, prog(prim_action, stoch_action, procedure)).
:- mode proc(in(ground), out(semidet_prog)) is det.
proc(P, P1) :-
    P = bla, P1 = nil.


:- instance bat(maze.prim_action, maze.stoch_action, maze.procedure) where [
    pred(poss/2) is maze.poss,
    pred(random_outcome/3) is maze.random_outcome,
    func(reward/1) is maze.reward,
    func(horizon/1) is maze.horizon,
    func(new_horizon/2) is maze.new_horizon,
    pred(proc/2) is maze.proc
].


:- func new_pos(prim_action, point) = point is det.
new_pos(A, P1) = P2 :-
    (   A = up,    P2 = north(P1)
    ;   A = down,  P2 = south(P1)
    ;   A = left,  P2 = west(P1)
    ;   A = right, P2 = east(P1)
    ).


:- func pos(sit(prim_action)) = point is det.
pos(S1) = P :-
    (   S1 = s0, P = start
    ;   S1 = do(A, S), P = new_pos(A, pos(S))
    ).

:- pred unvisited(point::in, sit(prim_action)::in) is semidet.
unvisited(P, S) :-
    unvisited(P, _, S).

:- pred unvisited(point::in, point::out, sit(prim_action)::in) is semidet.
unvisited(P1, P3, S1) :-
    (   S1 = s0, P3 = start
    ;   S1 = do(A, S), unvisited(P1, P2, S), P3 = new_pos(A, P2)
    ),
    P1 \= P3.

main(!IO) :-
    main2(!IO).

% I use this main predicate to test some hard-coded action sequences:
main1(!IO) :-
    RewMax = dist(start, goal),
    io.write(RewMax, !IO), io.nl(!IO),
    Pos0 = pos(s0),
    io.write(Pos0, !IO), io.nl(!IO),
    Up = pseudo_atom(atom(prim(up))),
    Down = pseudo_atom(atom(prim(down))),
    Left = pseudo_atom(atom(prim(left))),
    Right = pseudo_atom(atom(prim(right))),
    Prog = %Down `seq` Up `seq` Right `seq` Left `seq` % revisits same place --> failure
           Down `seq` Down `seq` Right `seq` Down `seq` Left `seq` Down `seq` Down `seq`
           Right `seq` Right `seq` Up `seq` Right `seq` Down `seq` Right `seq` Right,
    (   if      do(Prog, s0, S1)
        then    Rew1 = prgolog.reward(S1),
                Pos1 = pos(S1),
                ( if Rew1 = RewMax then Msg = "ok" else Msg = "failed early" ),
                io.format("%s\n", [s(Msg)], !IO),
                io.write(Rew1, !IO), io.nl(!IO),
                io.write(Pos1, !IO), io.nl(!IO),
                io.write(S1, !IO), io.nl(!IO)
        else    io.format("fail\n", [], !IO)
    ).

% Solve the maze using a program:
%    (up | down | left | right)*
main2(!IO) :-
    RewMax = dist(start, goal),
    io.write(RewMax, !IO), io.nl(!IO),
    Pos0 = pos(s0),
    io.write(Pos0, !IO), io.nl(!IO),
    Up = pseudo_atom(atom(prim(up))),
    Down = pseudo_atom(atom(prim(down))),
    Left = pseudo_atom(atom(prim(left))),
    Right = pseudo_atom(atom(prim(right))),
    Prog = star(Up `non_det` Down `non_det` Left `non_det` Right),
    (   if      do(Prog, s0, S1)
        then    Rew1 = prgolog.reward(S1),
                Pos1 = pos(S1),
                ( if Rew1 = RewMax then Msg = "ok" else Msg = "failed early" ),
                io.format("%s\n", [s(Msg)], !IO),
                io.write(Rew1, !IO), io.nl(!IO),
                io.write(Pos1, !IO), io.nl(!IO),
                io.write(S1, !IO), io.nl(!IO)
        else    io.format("fail\n", [], !IO)
    ).


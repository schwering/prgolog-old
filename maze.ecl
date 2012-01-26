% vim:ft=prolog:tw=80:sw=4:sts=4:et
%
% This is a maze basic action theory written in ECLiPSe-CLP.
% The idea is to compare the performance of Mercury to ECLiPSe-CLP.
%
% It is a copy of the Mercury implementation of the maze basic action theory
% with some (mostly syntactic) modifications to make it ECLiPSe-CLP compatible.
%
% I took the first one and stripped away the time and continuous change-specific
% parts, so that its functionality matches the Mercury implementation.
%
% Christoph Schwering (schwering@gmail.com)

:- module(maze).

:- use_module(prgolog).
:- use_module(worlds).
:- use_module(library(apply)).

:- export(horizon/1).
:- export(new_horizon/3).
:- export(primitive_action/1).
:- export(stochastic_action/1).
:- export(random_outcome/3).
:- export(fluent/1).
:- export(poss/2).
:- export(proc/2).

:- export(main/0).

room_size(20).
room_height(X) :- room_size(X).
room_width(X) :- room_size(X).

start(p(0, 0)).
goal(p(X, Y)) :- X is 2 * room_width - 1, Y is 2 * room_height - 1.

in_maze(p(X, Y)) :-
    0 =< X, X < 2 * room_width,
    0 =< Y, Y < 2 * room_height.

at_upper_wall(Y) :-  Y      mod room_height =:= 0.
at_lower_wall(Y) :- (Y + 1) mod room_height =:= 0.
at_left_wall(X)  :-  X      mod room_width  =:= 0.
at_right_wall(X) :- (X + 1) mod room_width  =:= 0.

at_td_door(X) :- X mod room_width =:= room_width // 2.
at_lr_door(Y) :- Y mod room_height =:= room_height // 2.

north(p(X, Y), P2) :- Y1 is Y-1, P2 = p(X, Y1).
south(p(X, Y), P2) :- Y1 is Y+1, P2 = p(X, Y1).
west(p(X, Y), P2) :- X1 is X-1, P2 = p(X1, Y).
east(p(X, Y), P2) :- X1 is X+1, P2 = p(X1, Y).

conn(P1, P2) :-
    P1 = p(X, Y),
    in_maze(P1),
    in_maze(P2),
    (   north(P1, P2), ( \+ at_upper_wall(Y) -> true ; at_td_door(X) )
    ;   south(P1, P2), ( \+ at_lower_wall(Y) -> true ; at_td_door(X) )
    ;   west(P1, P2),  ( \+ at_left_wall(X)  -> true ; at_lr_door(Y) )
    ;   east(P1, P2),  ( \+ at_right_wall(X) -> true ; at_lr_door(Y) )
    ).

dist(p(X1, Y1), p(X2, Y2), R) :-
    R is integer(floor(sqrt((X1-X2)^2 + (Y1-Y2)^2))).
%dist(p(X1, Y1), p(X2, Y2)) = abs(X1 - X2) + abs(Y1 - Y2).


primitive_action(left).
primitive_action(right).
primitive_action(up).
primitive_action(down).


stochastic_action(left).
stochastic_action(right).
stochastic_action(up).
stochastic_action(down).


poss(A, S) :-
    pos(P1, S),
    P2 is new_pos(A, P1),
    conn(P1, P2),
    unvisited(P2, S).


random_outcome(B, A, _S) :-
    (   B = s_up,    A = up
    ;   B = s_down,  A = down
    ;   B = s_left,  A = left
    ;   B = s_right, A = right
    ).



horizon(H) :- H is 5.


new_horizon(H, _C, H1) :- H1 is H - 1.


proc(P, P1) :- P = bla, P1 = nil.


new_pos(A, P1, P2) :-
    (   A = up,    P2 is north(P1)
    ;   A = down,  P2 is south(P1)
    ;   A = left,  P2 is west(P1)
    ;   A = right, P2 is east(P1)
    ).


fluent(pos(_)).
fluent(unvisited(_)).
fluent(unvisited(_, _)).
fluent(reward(_)).

pos(P, S1) :-
    (   S1 = s0, P is start
    ;   S1 = do(A, S), pos(OldPos, S), P is new_pos(A, OldPos)
    ).

unvisited(P, S) :- unvisited(P, _, S).

unvisited(P1, P3, S1) :-
    (   S1 = s0, P3 is start
    ;   S1 = do(A, S), unvisited(P1, P2, S), new_pos(A, P2, P3)
    ),
    P1 \= P3.

reward(R, S) :-
    Start is start,
    Goal is goal,
    pos(Pos, S),
    R is dist(Start, Goal) - dist(Pos, Goal).



main :-
    Start is start,
    Goal is goal,
    RewMax is dist(Start, Goal),
    pos(Pos0, s0),
    writeln(Pos0),
    Up = up,
    Down = down,
    Left = left,
    Right = right,
    Prog = star(Up # Down # Left # Right),
    %Prog = (Down ; Down ; Right ; Down),
    (
        do(reward, Prog, s0, S1)
    ->
        reward(Rew1, S1),
        pos(Pos1, S1),
        ( Rew1 =:= RewMax -> Msg = "ok" ; Msg = "failed early" ),
        writeln(Msg),
        writeln(Rew1),
        writeln(Pos1),
        writeln(S1)
    ;
        writeln("fail")
    ).

:- add_world(maze).


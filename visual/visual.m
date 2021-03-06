%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: visual.m.
% Main author: schwering.
%
% Provides a handler for the online plan recognition procedure that visualizes
% a given sample of the plan recognition in the continuous car world.
%
% Visualization is done with ncurses.
% A simple street is drawn, the two agents are displayed, and the sample's
% phase (running, finished with failure, finished with success) is displayed.
%
%-----------------------------------------------------------------------------%

:- module visual.

:- interface.

:- import_module io.
:- import_module domain.
:- import_module domain.car.
:- import_module domain.car.cont.
:- import_module list.
:- use_module planrecog.

%-----------------------------------------------------------------------------%

%:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type area.
:- type areas == list(area).

:- pred init(int::in, areas::out, io::di, io::uo) is det.

:- pred finish(io::di, io::uo) is det.

:- pred clear(area::in, io::di, io::uo) is det.

:- pred visualize(areas) `with_type` planrecog.handler(prim).
:- mode visualize(in)    `with_inst` planrecog.handler.

:- pred wait_for_key(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module domain.car.cont.io_util.
:- import_module char.
:- import_module curs.
:- import_module curs.panel.
:- import_module float.
:- import_module int.
:- import_module prgolog.
:- import_module prgolog.ccfluent.
:- import_module prgolog.nice.
:- import_module string.
:- import_module solutions.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type area ---> area(top    :: int,
                       left   :: int,
                       rows   :: int,
                       cols   :: int,
                       width  :: m,
                       length :: m).

:- type mapsit == {assoc_list(var, number), sit(prim)}.

%-----------------------------------------------------------------------------%

:- func bottom(area) = int is det.

bottom(Area) = top(Area) + rows(Area) - 1.


:- func right(area) = int is det.

right(Area) = left(Area) + cols(Area) - 1.

%-----------------------------------------------------------------------------%

:- pred get_data(mapsit::in, agent::in,
                 s::out, m::out, m::out, m::out, m::out) is det.

get_data({Map, S}, Agent, Time, ModX, ModY, ObsX, ObsY) :-
    (   if      some [X, Y] (
                    S = do(match(car_obs(Obs), _, _, _), _),
                    X = x_pos(Obs, Agent),
                    Y = y_pos(Obs, Agent)
                )
        then    ObsX = X, ObsY = Y
        else    ObsX = -1.0, ObsY = -1.0
    ),
    E = ( func(T) = eval_float(Map, T) ),
    Time = E(start(S)),
    ModX = E(x(Agent, S)(start(S))),
    ModY = E(y(Agent, S)(start(S))).


:- func agent_to_color(agent::in) = (colour::out) is det.

agent_to_color(b) = red.
agent_to_color(c) = blue.
agent_to_color(d) = green.
agent_to_color(e) = magenta.
agent_to_color(f) = cyan.
agent_to_color(g) = yellow.
agent_to_color(h) = white.


:- pred border(area::in, io::di, io::uo) is det.

border(Area, !IO) :-
    Top = top(Area),
    Left = left(Area),
    Rows = rows(Area),
    Cols = cols(Area),
    move(Top + 0,        Left + 1, !IO), hline(acs_hline, Cols - 2, !IO),
    move(Top + Rows - 1, Left + 1, !IO), hline(acs_hline, Cols - 2, !IO),
    %move(Top + 1,        Left + 0,        !IO), vline(acs_hline, Rows - 2, !IO),
    %move(Top + 1,        Left + Cols - 1, !IO), vline(acs_hline, Rows - 2, !IO),
    %move(top(Area), left(Area), !IO), addch(normal, to_int('+'), !IO),
    %move(top(Area), right(Area), !IO), addch(normal, to_int('+'), !IO),
    %move(bottom(Area), left(Area), !IO), addch(normal, to_int('+'), !IO),
    %move(bottom(Area), right(Area), !IO), addch(normal, to_int('+'), !IO),
    true.


clear(Area, !IO) :- clear_2(Area, top(Area), cols(Area), !IO).


:- pred clear_2(area::in, int::in, int::in, io::di, io::uo) is det.

clear_2(Area, ThisRow, Cols, !IO) :-
    move(ThisRow, left(Area), !IO),
    hline(to_int(' '), Cols, !IO),
    (   if      ThisRow < bottom(Area)
        then    clear_2(Area, ThisRow + 1, Cols, !IO)
        else    true
    ).


:- pred draw_center_line(area::in, io::di, io::uo) is det.

draw_center_line(Area, !IO) :-
    move(top(Area) + rows(Area) / 2, left(Area) + 1, !IO),
    hline(to_int('-'), cols(Area) - 2, !IO).


:- pred draw_phase_name(area::in, planrecog.s_phase::in, io::di, io::uo) is det.

draw_phase_name(Area, Phase, !IO) :-
    (   Phase = planrecog.running,   S = "running",   Atr = fg_bg(yellow, black)
    ;   Phase = planrecog.finishing, S = "finishing", Atr = fg_bg(yellow, black)
    ;   Phase = planrecog.finished,  S = "finished",  Atr = fg_bg(green, black)
    ;   Phase = planrecog.failed,    S = "failed",    Atr = fg_bg(red, black)
    ),
    move(top(Area) + 1, left(Area) + 1, !IO),
    addstr(Atr, S, !IO).


:- pred draw_time(area::in, s::in, io::di, io::uo) is det.

draw_time(Area, Time, !IO) :-
    move(top(Area) + 1, left(Area) + 5 + length("finished"), !IO),
    addstr(normal, "t = " ++ format("%4.1f", [f(Time)]), !IO).


:- pred draw_point(area::in, attr::in, agent::in,
                   m::in, m::in, io::di, io::uo) is det.

draw_point(Area, Attr, Agent, X, Y, !IO) :-
    W = float(rows(Area)) / 2.0,
    Row = truncate_to_int(-1.0 * Y / width(Area) * W + W),
    Col = truncate_to_int(X / length(Area) * float(cols(Area))) + 1,
    move(top(Area) + Row, left(Area) + Col, !IO),
    addstr(Attr + fg_bg(agent_to_color(Agent), black),
           to_upper(agent_to_string(Agent)), !IO).


:- pred draw_data(mapsit::in, area::in, agent::in, io::di, io::uo) is det.

draw_data(MapSit, Area, Agent, !IO) :-
    get_data(MapSit, Agent, Time, ModX, ModY, _ObsX, _ObsY),
    draw_time(Area, Time, !IO),
    draw_point(Area, bold, Agent, ModX, ModY, !IO),
    true.% draw_point(Area, underline, Agent, ObsX, ObsY, !IO).


:- pred draw_sit(mapsit::in, area::in, io::di, io::uo) is det.

draw_sit(MapSit, Area, !IO) :-
    draw_center_line(Area, !IO),
    aggregate(agent, draw_data(MapSit, Area), !IO),
    border(Area, !IO).


:- pred draw_null_sit(area::in, io::di, io::uo) is det.

draw_null_sit(Area, !IO) :-
    draw_center_line(Area, !IO),
    border(Area, !IO).


:- pred create_area_row(int::in, int::in, int::in, int::in,
                        list(area)::out, io::di, io::uo) is det.

create_area_row(Rows, Cols, Row, Col, Areas, !IO) :-
    rows_cols(_, MaxCols, !IO),
    (   if      Col + Cols =< MaxCols
        then    Area = area(Row, Col, Rows, Cols, 5.0, 750.0),
                draw_null_sit(Area, !IO),
                create_area_row(Rows, Cols, Row, Col + Cols, Areas0, !IO),
                Areas = [Area | Areas0]
        else    Areas = []
    ).


:- pred draw_sits_on_areas(list(sit(prim))::in, list(area)::in,
                           io::di, io::uo) is det.

draw_sits_on_areas([], _, !IO).
draw_sits_on_areas([S | Sits], [Area | Areas], !IO) :-
    if      solve(vargen(S), constraints(S), Map, _Val)
    then    draw_sit({Map, S}, Area, !IO),
            draw_sits_on_areas(Sits, Areas, !IO)
    else    draw_null_sit(Area, !IO).
draw_sits_on_areas([_ | _], [], !IO) :-
    error("not enough areas for mapsits").


init(N, Areas, !IO) :-
    start(!IO),
    rows_cols(MaxRows, MaxCols, !IO),
    NStacks = 3,
    Rows = ( if (MaxRows / NStacks) mod 2 = 0 then MaxRows / NStacks - 1 else MaxRows / NStacks ),
    Cols = MaxCols / ((N + NStacks - 1) / NStacks),
    create_area_row(Rows, Cols, 0 * Rows, 0, Areas0, !IO),
    create_area_row(Rows, Cols, 1 * Rows, 0, Areas1, !IO),
    create_area_row(Rows, Cols, 2 * Rows, 0, Areas2, !IO),
    Areas = reverse(Areas0) ++ reverse(Areas1) ++ reverse(Areas2),
    refresh(!IO).


finish(!IO) :-
    stop(!IO).


visualize(Areas, I, !State, !IO) :-
    lock(!IO),
    !.State = planrecog.s_state(Conf, Phase),
    Conf = conf(_Prog, Sit),
    (   if      index1(Areas, I, Area)
        then    clear(Area, !IO),
                draw_phase_name(Area, Phase, !IO),
                (   if      solve(vargen(Sit), constraints(Sit), Map, _Val)
                    then    draw_sit({Map, Sit}, Area, !IO)
                    else    draw_null_sit(Area, !IO)
                ),
                move(bottom(Area), right(Area), !IO),
                refresh(!IO)
        else   true
    ),
    unlock(!IO).


:- pragma foreign_decl("C", "
    static pthread_mutex_t ncurses_mutex = PTHREAD_MUTEX_INITIALIZER;
").


:- pred lock(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    lock(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    pthread_mutex_lock(&ncurses_mutex);
    IO1 = IO0;
").


:- pred unlock(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    unlock(IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    pthread_mutex_unlock(&ncurses_mutex);
    IO1 = IO0;
").


wait_for_key(!IO) :-
    getch(_, !IO).


:- finalize stop/2.


%:- pred curs_main(io::di, io::uo) is det.
%
%curs_main(!IO) :-
%    visualize_sits([{[], s0}], 1, 1, Areas, !IO),
%    update_panels(!IO),
%    getch(_, !IO).


%main(!IO) :-
%    start(!IO),
%    Area = area(2, 2, 10, 50, 1.0, 1.0),
%    border(Area, !IO),
%    addstr(fg_bg(white, black), "huhu", !IO),
%    %move(Panel, 3, 3, !IO),
%    %addstr(Panel, normal, "huhu", !IO),
%    %move(Panel, 6, 6, !IO),
%    %addstr(Panel, normal, "haha", !IO),
%    refresh(!IO),
%    getch(_, !IO),
%    stop(!IO).

%-----------------------------------------------------------------------------%
:- end_module visual.
%-----------------------------------------------------------------------------%

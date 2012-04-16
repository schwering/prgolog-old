%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: visual.m.
% Main author: schwering.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module visual.

:- interface.

:- import_module bat.
:- import_module io.
:- import_module prgolog.

%-----------------------------------------------------------------------------%

%:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type area.
:- type areas.

:- pred init_visual(int::in, areas::out, io::di, io::uo) is det.
:- pred finish_visual(io::di, io::uo) is det.
:- pred visualize(areas::in, int::in, sit(prim)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module char.
:- import_module curs.
:- import_module curs.panel.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module prgolog.ccfluent.
:- import_module string.
:- import_module types.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type area ---> area(panel  :: panel,
                       rows   :: int,
                       cols   :: int,
                       width  :: m,
                       length :: m).

:- type areas == list(area).

:- type mapsit == {assoc_list(var, number), sit(prim)}.

%-----------------------------------------------------------------------------%

:- pred get_data(mapsit::in, agent::in,
                 s::out, m::out, m::out, m::out, m::out) is det.

get_data({Map, S}, Agent, Time, ModX, ModY, ObsX, ObsY) :-
    (   if      S = do(match(Obs, _, _, _), _)
        then    (   if      Obs = {_, Agent, X0, Y0, _, _, _}
                    then    ObsX = X0,
                            ObsY = Y0
                    else if Obs = {_, _, _, _, Agent, X0, Y0}
                    then    ObsX = X0,
                            ObsY = Y0
                    else    error("invalid observation does not contain driver")
                )
        else    ObsX = -1.0, ObsY = -1.0
    ),
    E = ( func(T) = eval_float(Map, T) ),
    Time = E(start(S)),
    ModX = E(x(Agent, S)(start(S))),
    ModY = E(y(Agent, S)(start(S))).


:- func color(agent::in) = (colour::out) is det.

color(a) = red.
color(b) = blue.


:- pred draw_center_line(area::in, io::di, io::uo) is det.

draw_center_line(Area, !IO) :-
    move(panel(Area), rows(Area) / 2, 1, !IO),
    hline(panel(Area), to_int('-'), cols(Area) - 2, !IO).


:- pred draw_time(area::in, s::in, io::di, io::uo) is det.

draw_time(Area, Time, !IO) :-
    move(panel(Area), 1, 1, !IO),
    addstr(panel(Area), normal, "t = " ++ float_to_string(Time), !IO).


:- pred draw_point(area::in, attr::in, agent::in,
                   m::in, m::in, io::di, io::uo) is det.

draw_point(Area, Attr, Agent, X, Y, !IO) :-
    W = float(rows(Area)) / 2.0,
    Row = truncate_to_int(Y / width(Area) * W + W),
    Col = truncate_to_int(X / length(Area) * float(cols(Area))) + 1,
    move(panel(Area), Row, Col, !IO),
    addstr(panel(Area), Attr + fg_bg(color(Agent), black),
           agent_to_string(Agent), !IO).


:- pred draw_data(mapsit::in, area::in, agent::in, io::di, io::uo) is det.

draw_data(MapSit, Area, Agent, !IO) :-
    get_data(MapSit, Agent, Time, ModX, ModY, ObsX, ObsY),
    draw_time(Area, Time, !IO),
    draw_point(Area, normal, Agent, ModX, ModY, !IO),
    draw_point(Area, underline, Agent, ObsX, ObsY, !IO).


:- pred draw_sit(mapsit::in, area::in, io::di, io::uo) is det.

draw_sit(MapSit, Area, !IO) :-
    clear(panel(Area), !IO),
    draw_center_line(Area, !IO),
    draw_data(MapSit, Area, a, !IO),
    draw_data(MapSit, Area, b, !IO),
    border(panel(Area), !IO).


:- pred draw_null_sit(area::in, io::di, io::uo) is det.

draw_null_sit(Area, !IO) :-
    clear(panel(Area), !IO),
    draw_center_line(Area, !IO),
    border(panel(Area), !IO).


:- pred visualize_sit(mapsit::in,
                      int::in, int::in,
                      int::in, int::in,
                      area::out,
                      io::di, io::uo) is det.

visualize_sit(MapSit, RowOffset, ColOffset, NRows, NCols, Area, !IO) :-
    rows_cols(MaxRow, MaxCol, !IO),
    Row = min(RowOffset, MaxRow - 2),
    Col = min(ColOffset, MaxCol - 2),
    Rows = min(NRows, MaxRow - RowOffset),
    Cols = min(NCols, MaxCol - ColOffset),
    Attr = fg_bg(white, black),
    new(Rows, Cols, Row, Col, Attr, Panel, !IO),
    Area = area(Panel, Rows, Cols, 5.0, 750.0),
    draw_sit(MapSit, Area, !IO).


:- pred visualize_sits(list(mapsit)::in, int::in, int::in, list(area)::out,
                       io::di, io::uo) is det.

visualize_sits([], _, _, [], !IO).
visualize_sits([MapSit | MapSits], RowOffset, ColOffset, [Area | Areas], !IO) :-
    visualize_sit(MapSit, RowOffset, ColOffset, 21, 80, Area, !IO),
    visualize_sits(MapSits, RowOffset + 1, ColOffset + 1, Areas, !IO).


:- pred create_area_rows(int::in, int::in, int::in, int::in,
                         list(area)::out, io::di, io::uo) is det.

create_area_rows(Rows, Cols, Row, Col, Areas, !IO) :-
    rows_cols(_, MaxCols, !IO),
    (   if      Col + Cols =< MaxCols
        then    Attr = fg_bg(white, black),
                new(Rows, Cols, Row, Col, Attr, Panel, !IO),
                Area = area(Panel, Rows, Cols, 5.0, 750.0),
                draw_null_sit(Area, !IO),
                create_area_rows(Rows, Cols, Row, Col + Cols, Areas0, !IO),
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


init_visual(N, Areas, !IO) :-
    start(!IO),
    rows_cols(MaxRows, MaxCols, !IO),
    NStacks = 3,
    Rows = ( if (MaxRows / NStacks) mod 2 = 0 then MaxRows / NStacks - 1 else MaxRows / NStacks ),
    Cols = MaxCols / ((N + NStacks - 1) / NStacks),
    create_area_rows(Rows, Cols, 0 * Rows, 0, Areas0, !IO),
    create_area_rows(Rows, Cols, 1 * Rows, 0, Areas1, !IO),
    create_area_rows(Rows, Cols, 2 * Rows, 0, Areas2, !IO),
    Areas = reverse(Areas0) ++ reverse(Areas1) ++ reverse(Areas2),
    update_panels(!IO).


finish_visual(!IO) :-
    stop(!IO).


visualize(Areas, I, S, !IO) :-
    Area = det_index1(Areas, I),
    (   if      solve(vargen(S), constraints(S), Map, _Val)
        then    draw_sit({Map, S}, Area, !IO)
        else    draw_null_sit(Area, !IO)
    ),
    update_panels(!IO).


%:- pred curs_main(io::di, io::uo) is det.
%
%curs_main(!IO) :-
%    visualize_sits([{[], s0}], 1, 1, Areas, !IO),
%    update_panels(!IO),
%    getch(_, !IO).


%main(!IO) :-
%    init_visual(10, _, !IO),
%    getch(_, !IO),
%    finish_visual(!IO).

%-----------------------------------------------------------------------------%
:- end_module visual.
%-----------------------------------------------------------------------------%

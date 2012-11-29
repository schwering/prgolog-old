%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.fuzzy.test.m.
% Main author: schwering.
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.fuzzy.test.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred test_ntg(io::di, io::uo) is det.
:- pred test_ttc(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module arithmetic.
:- import_module bool.
:- import_module exception.
:- import_module list.
:- use_module math.
:- import_module prgolog.nice.
:- import_module string.
:- import_module rat.
:- import_module require.

%-----------------------------------------------------------------------------%

test_ntg(!IO) :-
    In = (pred(Val::in, Cat::in) is det :-
        if Val `in` Cat then true else throw({Val, Cat})
    ),
    NotIn = (pred(Val::in, Cat::in) is det :-
        if Val `not_in` Cat then true else throw({Val, Cat})
    ),
    In(   -100.0,   very_far_infront),
    In(     -9.0,   very_far_infront),
    In(     -8.0,   very_far_infront),
    In(     -7.0,   very_far_infront),
    In(     -6.0,   very_far_infront),
    In(     -5.1,   very_far_infront),
    NotIn(  -5.0,   very_far_infront),
    NotIn(  -7.0,        far_infront),
    In(     -6.9,        far_infront),
    In(     -6.0,        far_infront),
    In(     -5.0,        far_infront),
    In(     -4.9,        far_infront),
    In(     -4.0,        far_infront),
    NotIn(  -4.0,            infront),
    In(     -3.9,            infront),
    In(     -2.1,            infront),
    NotIn(  -2.0,            infront),
    NotIn(  -2.6,      close_infront),
    NotIn(  -2.5,      close_infront),
    In(     -2.4,      close_infront),
    In(     -2.0,      close_infront),
    In(     -1.5,      close_infront),
    In(     -1.1,      close_infront),
    NotIn(  -1.0,      close_infront),
    NotIn(  -1.5, very_close_infront),
    In(     -1.4, very_close_infront),
    In(     -1.1, very_close_infront),
    In(     -1.0, very_close_infront),
    In(     -0.9, very_close_infront),
    NotIn(  -0.5, very_close_infront),
    NotIn(  -0.8,       side_by_side),
    In(     -0.7,       side_by_side),
    In(     -0.1,       side_by_side),
    In(      0.0,       side_by_side),
    In(      0.1,       side_by_side),
    In(      0.7,       side_by_side),
    NotIn(   0.8,       side_by_side),
    NotIn(   0.5,  very_close_behind),
    In(      0.9,  very_close_behind),
    In(      1.0,  very_close_behind),
    In(      1.1,  very_close_behind),
    In(      1.4,  very_close_behind),
    NotIn(   1.5,  very_close_behind),
    NotIn(   1.0,       close_behind),
    In(      1.1,       close_behind),
    In(      2.0,       close_behind),
    In(      2.4,       close_behind),
    NotIn(   2.5,       close_behind),
    NotIn(   2.0,             behind),
    In(      2.5,             behind),
    In(      3.0,             behind),
    In(      3.5,             behind),
    NotIn(   4.0,             behind),
    NotIn(   3.0,         far_behind),
    In(      4.0,         far_behind),
    In(      5.0,         far_behind),
    In(      5.5,         far_behind),
    NotIn(   7.0,         far_behind),
    NotIn(   5.0,    very_far_behind),
    In(      6.0,    very_far_behind),
    In(      6.0,    very_far_behind),
    In(      9.0,    very_far_behind),
    In(     10.0,    very_far_behind),
    In(    100.0,    very_far_behind),
    ( if -0.7 `in_all` [very_close_infront, side_by_side]
      then true else throw({"in_all", -0.7}) ),
    ( if 0.0 `in_any` [very_close_infront, side_by_side, very_close_behind]
      then true else throw({"in_any", 0.0}) ),
    ( if 0.0 `in_none` [very_close_infront, very_close_behind]
      then true else throw({"in_none", 0.0}) ),
    true.

test_ttc(!IO) :-
    In = (pred(Val::in, Cat::in) is det :-
        if Val `in` Cat then true else throw({Val, Cat})
    ),
    NotIn = (pred(Val::in, Cat::in) is det :-
        if Val `not_in` Cat then true else throw({Val, Cat})
    ),
    In(   -100.0,   expanding_slowly),
    In(    -15.0,   expanding_slowly),
    In(    -11.0,   expanding_slowly),
    NotIn( -10.0,   expanding_slowly),
    NotIn( -13.0,   expanding),
    NotIn( -12.0,   expanding),
    In(    -11.0,   expanding),
    In(     -7.0,   expanding),
    In(     -4.0,   expanding),
    NotIn(  -3.5,   expanding),
    NotIn(  -5.0,   expanding_fast),
    In(     -4.0,   expanding_fast),
    In(     -2.5,   expanding_fast),
    NotIn(   0.0,   expanding_fast),
    NotIn(  -2.0,   reached),
    In(     -1.9,   reached),
    In(      1.9,   reached),
    NotIn(   2.0,   reached),
    NotIn(   0.0,   contracting_fast),
    In(      2.0,   contracting_fast),
    In(      3.0,   contracting_fast),
    NotIn(   5.0,   contracting_fast),
    NotIn(   3.5,   contracting),
    In(      7.0,   contracting),
    In(      9.0,   contracting),
    NotIn(  12.0,   contracting),
    NotIn(  10.0,   contracting_slowly),
    In(     10.5,   contracting_slowly),
    In(     14.0,   contracting_slowly),
    In(     15.0,   contracting_slowly),
    In(     20.0,   contracting_slowly),
    true.

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.fuzzy.test.
%-----------------------------------------------------------------------------%

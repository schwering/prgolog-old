%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012-2013 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.car.rstc.progression.m.
% Main author: schwering.
%
% Simple progression interface.
% The progress/2 predicate simply queries the start/1, reward/1, NTG/3, TTC/3,
% and lane/2 functional fluents and stores the result in a situation term
%   do(init_env(rel_env(_, _, _, _, _, _)), s0).
%
% progress/4 additionally calls reset_memo/2 for some tabled predicates.
% When memoization is enabled or disabled for fluents, this should obvioulsy
% be adjusted.
%
% Furthermore progress/4 stores the old situation term in a mutable variable
% so that unprogress/4 can rebuild the situation term.
%
% XXX: sitlen/1's value is not stored in rel_env and thus gets lost. In general
% I think this progression mechanism is too fragile for now. We'd need some
% way to register fluents or so (that should be easy with higher order
% functions and predicates, but we can't use them as keys in the retrieval).
%
%-----------------------------------------------------------------------------%

:- module domain.car.rstc.progression.

:- interface.

:- use_module util.arithmetic.

%-----------------------------------------------------------------------------%

:- type ground_number == float.

%-----------------------------------------------------------------------------%

:- pred progress(rstc.sit(ground_number), rstc.sit(ground_number))
    <= arithmetic.arithmetic(ground_number).
:- mode progress(in(finite_time_sit), out) is det.
:- mode progress(in, out) is semidet.

:- pred progress(rstc.sit(ground_number), rstc.sit(ground_number), io, io)
    <= arithmetic.arithmetic(ground_number).
:- mode progress(in, out, di, uo) is det.

:- pred unprogress(rstc.sit(ground_number), rstc.sit(ground_number), io, io)
    <= arithmetic.arithmetic(ground_number).
:- mode unprogress(in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

%:- include_module test.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module domain.car.rstc.bat.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- mutable(sit, rstc.sit(ground_number), s0, ground,
           [attach_to_io_state, untrailed]).

%-----------------------------------------------------------------------------%

progress(S, do(init_env(rel_env(Time, NTGs, TTCs, Lanes, Reward, Sitlen)), s0)) :-
    Time = start(S),
    Reward = reward(S),
    solutions(agent, Agents),
    NTGs = condense(map(func(B) = condense(map(func(C) =
        ( if NTG = ntg(B, C, S) then [pair({B, C}, NTG)] else [] )
    , Agents)), Agents)),
    TTCs = condense(map(func(B) = condense(map(func(C) =
        ( if TTC = ttc(B, C, S) then [pair({B, C}, TTC)] else [] )
    , Agents)), Agents)),
    Lanes = map(func(B) = pair(B, lane(B, S)), Agents),
    Sitlen = sitlen(S).


progress(S, S1, !IO) :-
    if      progress(S, S0)
    then    save_sit(S, !IO),
            bat.reset_memo(!IO),
            rstc.reset_memo(!IO),
            S1 = S0
    else    unexpected($module, "progression failed (start(S) undefined?)").


:- pred save_sit(rstc.sit(ground_number)::in, io::di, io::uo) is det
    <= arithmetic.arithmetic(ground_number).

save_sit(S, !IO) :-
    unprogress(S, S1, !IO),
    set_sit(S1, !IO).


unprogress(S, S1, !IO) :-
    get_sit(S0, !IO),
    S1 = unprogress(S, S0).


:- func unprogress(rstc.sit(N), rstc.sit(N)) = rstc.sit(N).

unprogress(s0, S0) = S0.
unprogress(do(A, S), S0) =
    (   if   A = init_env(rel_env(_, _, _, _, _, _))
        then unprogress(S, S0)
        else do(A, unprogress(S, S0))
    ).

%-----------------------------------------------------------------------------%
:- end_module domain.car.rstc.progression.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: domain.m.
% Main author: schwering.
%
% Extensions of the prGolog BAT typeclass for plan recognition.
%
%-----------------------------------------------------------------------------%

:- module domain.

:- interface.

:- import_module io.
:- import_module prgolog.

%-----------------------------------------------------------------------------%

:- typeclass obs_bat(A, Obs) <= ((A -> Obs), bat(A)) where [
    pred is_obs_action(A),
    mode is_obs_action(in) is semidet,

    pred is_obs_prog(pseudo_atom(A)),
    mode is_obs_prog(in) is semidet,

    pred covered_by_obs(sit(A)),
    mode covered_by_obs(in) is semidet,

    func obs_to_action(Obs) = pseudo_atom(A),
    mode obs_to_action(in) = out is det
].

%-----------------------------------------------------------------------------%

:- typeclass pr_bat(A, Obs, Env) <= ((A, Obs -> Env), obs_bat(A, Obs)) where [
    func seed_init_sit(int) = sit(A),
    mode seed_init_sit(in) = out is det,

    func init_env_sit(Env, sit(A)) = sit(A),
    mode init_env_sit(in, in) = out is det
].

%-----------------------------------------------------------------------------%

:- func reward(sit(A)) = reward <= bat(A).

%-----------------------------------------------------------------------------%

:- type obs_msg(Obs, Env) ---> init_msg(Env)
                          ;    obs_msg(Obs)
                          ;    end_of_obs.

:- type activity ---> unused
                 ;    working
                 ;    finished
                 ;    failed.

:- type stream == int.

:- typeclass obs_source(Obs, Env, Source, StreamState)
        <= ((Source -> StreamState),
            (StreamState -> Source),
            (Source, StreamState -> Obs, Env),
            (Obs, Env -> Source, StreamState)) where [
    pred reset_obs_source(Source, io, io),
    mode reset_obs_source(in, di, uo) is det,

    pred init_obs_stream(Source, stream, StreamState, io, io),
    mode init_obs_stream(in, in, uo, di, uo) is det,

    pred next_obs(obs_msg(Obs, Env), sit(A), prog(A),
                  StreamState, StreamState, io, io) <= pr_bat(A, Obs, Env),
    mode next_obs(out, in, in, di, uo, di, uo) is det,

    pred mark_obs_end(Source, io, io),
    mode mark_obs_end(in, di, uo) is det,

    pred update_state(Source, stream, activity, io, io),
    mode update_state(in, in, in, di, uo) is det
].

%-----------------------------------------------------------------------------%

:- include_module car.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.

%-----------------------------------------------------------------------------%

reward(s0) = 0.0.
reward(do(A, S)) = reward(A, S) + reward(S).

%-----------------------------------------------------------------------------%
:- end_module domain.
%-----------------------------------------------------------------------------%

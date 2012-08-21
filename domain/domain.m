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
%-----------------------------------------------------------------------------%

:- module domain.

:- interface.

:- import_module io.
:- import_module prgolog.

%-----------------------------------------------------------------------------%

:- typeclass obs_bat(A, B, P, Obs) <= ((A, B, P -> Obs), bat(A, B, P)) where [
    pred is_obs(A),
    mode is_obs(in) is semidet,

    pred covered_by_obs(sit(A)),
    mode covered_by_obs(in) is semidet,

    func obs_to_action(Obs) = A,
    mode obs_to_action(in) = out is det
].

%-----------------------------------------------------------------------------%

:- typeclass pr_bat(A, B, P, Obs, Env)
        <= ((A, B, P, Obs -> Env),
            (Obs, Env -> A, B, P),
            obs_bat(A, B, P, Obs)) where [
    func seed_init_sit(int) = sit(A),
    mode seed_init_sit(in) = out is det,

    func init_env_sit(Env, sit(A)) = sit(A),
    mode init_env_sit(in, in) = out is det
].

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

    pred init_obs_stream(Source, stream, StreamState),
    mode init_obs_stream(in, in, uo) is det,

    pred next_obs(obs_msg(Obs, Env), sit(A), prog(A, B, P),
                  StreamState, StreamState) <= pr_bat(A, B, P, Obs, Env),
    mode next_obs(out, in, in, di, uo) is det,

    pred mark_obs_end(Source, io, io),
    mode mark_obs_end(in, di, uo) is det,

    pred update_state(Source, stream, activity, io, io),
    mode update_state(in, in, in, di, uo) is det
].

%-----------------------------------------------------------------------------%

:- include_module car.

%-----------------------------------------------------------------------------%
:- end_module domain.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%-----------------------------------------------------------------------------%
%
% File: cars_server.m.
% Main author: schwering.
%
% Plan recognition server. Accepts TCP connections on port 19123. The protocol
% is then a loop of the following two steps:
% (1) the server receives (syscall read) an observation (struct observation_record)
% (2) the server answers (syscall write) the state (struct planrecog_state).
% The communication parts are written in C. This probably makes the
% serialization simpler.
%
%-----------------------------------------------------------------------------%

:- module cars_server.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module domain.
:- import_module domain.car.
%:- import_module domain.car.cont.
:- import_module domain.car.rstc.
:- import_module domain.car.rstc.bat.
:- import_module domain.car.rstc.debug_bat.
%:- import_module domain.car.rstc.progression.
:- import_module domain.car.obs.
:- import_module domain.car.obs.torcs.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module planrecog.
:- import_module prgolog.
:- import_module prgolog.debug.
:- import_module prgolog.nice.
:- import_module string.
:- import_module util.
:- use_module util.arithmetic.
:- import_module util.arithmetic.impl.
:- import_module util.time.
:- use_module visual.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include <assert.h>
    #include <netinet/in.h>
    #include <string.h>
    #include <stdio.h>
    #include <strings.h>
    #include <sys/types.h>
    #include <sys/socket.h>
    #include <unistd.h>
    #include ""domain-car-obs-torcs-types.h""
").


:- pred make_server_socket(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    make_server_socket(Socket::out, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    struct sockaddr_in server_addr;
    Socket = socket(AF_INET, SOCK_STREAM, 0);
    if (Socket < 0) {
        fprintf(stderr, ""Couldn't open socket\\n"");
        exit(1);
    }
    bzero((char*) &server_addr, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(PORT);
    if (bind(Socket, (struct sockaddr*) &server_addr, sizeof(server_addr)) < 0) {
        fprintf(stderr, ""Couldn't bind socket\\n"");
        exit(1);
    }
    listen(Socket, 1);
    IO1 = IO0;
").


:- pred accept_connection(int::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    accept_connection(ServerSocket::in, Socket::out, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);
    Socket = accept(ServerSocket, (struct sockaddr*) &client_addr, &client_len);
    if (Socket < 0) {
      fprintf(stderr, ""Couldn't accept connection\\n"");
      exit(1);
    }
    IO1 = IO0;
").


:- pred finalize_connection(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    finalize_connection(Socket::in, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    close(Socket);
    IO1 = IO0;
").


:- pred handle_connection(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    handle_connection(Socket::in, IO0::di, IO1::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    for (;;) {
        struct observation_record obs;
        struct planrecog_state msg;
        int ret;
        ret = read(Socket, &obs, sizeof(obs));
        if (ret != sizeof(obs)) {
            break;
        }
        domain__car__obs__torcs__push_obs(&obs);

        domain__car__obs__torcs__init_msg(&msg);
        ret = write(Socket, &msg, sizeof(msg));
        if (ret != sizeof(msg)) {
            break;
        }
    }
    IO1 = IO0;
").

%-----------------------------------------------------------------------------%

:- func sit2list(prgolog.sit(A)) = list(A) is det.

sit2list(s0) = [].
sit2list(do(A, S)) = [A|sit2list(S)].


:- pred debug_conf(rstc.prog(N)::in, rstc.sit(N)::in,
                   io::di, io::uo) is det <= arithmetic.arithmetic(N).

debug_conf(P, S, !IO) :-
    (
        if      some [S0, P1, S1, _P2, S2]
                ( S = do(_, S0), trans(P, S0, P1, S1), trans(P1, S1, _P2, S2) )
        then    format("trans/4 succeeded\n", [], !IO),
                format("reward S: %f\n", [f(reward(S))], !IO),
                format("reward S0: %f\n", [f(reward(S0))], !IO),
                format("reward S1: %f\n", [f(reward(S1))], !IO),
                format("reward S2: %f\n", [f(reward(S2))], !IO)
        else    format("trans/4 failed\n", [], !IO)
    ),
    (
        if      n_trans(2, wrap_prog(P), wrap_sit(S), _, _)
        then    format("n_trans/5 succeeded\n", [], !IO)
        else    format("n_trans/5 failed\n", [], !IO)
    ).


:- pred stdout_handler(source, int) `with_type` handler(rstc.prim(float))
    <= arithmetic.arithmetic(float).
:- mode stdout_handler(in, in) `with_inst` handler.
%:- mode stdout_handler(ui, in) `with_inst` handler.

stdout_handler(Source, N, I,
               s_state(conf(P, S), Phase),
               s_state(conf(P, S1), Phase), !IO) :-
    Fmt = (pred(FormatString::in, Args::in, !.SubIO::di, !:SubIO::uo) is det :-
        format("%d.%d: " ++ FormatString, [i(N), i(I)] ++ Args, !SubIO)
    ),
    nl(!IO),
    Fmt("%f =< p_%d =< %f\n", [f(min_confidence(Source)), i(I), f(max_confidence(Source))], !IO),
    (
        ( Phase = running ; Phase = finishing ),
        Fmt("Running or Finishing: got a new observation "++
            "(buffered/lookahead: %d/%d) or executed a action\n",
            [i(obs_trans_count(P)), i(lookahead(S))], !IO),
        Fmt("    Reward: %s\n", [s(string(reward(S)))], !IO),
        (   if      S = do(A, _)
            then    Fmt("    Last action: %s\n",
                           [s(string(A))], !IO)
            else    true
        ),
        Fmt("    Remaining program:\n", [], !IO),
        Fmt("        %s\n", [s(string(P))], !IO),
        print_prog(Fmt, P, !IO),
        %print_decomps(S, P, !IO),
        %foldl((pred(decomp(C, R)::in, !.SubIO::di, !:SubIO::uo) is det :-
        %    Fmt("    *** %s\n", [s(if C = primf(AF) then string(AF(S)) else string(C))], !SubIO)
        %), tree_to_list(tree.force(pickbest(S), next2(P))), !IO)
        % XXX Interesting Mercury bug (or what else?):
        % Let's assume that P contains a star(pickbest(...)).
        % In the following code, next2(P) has inst non-strict.
        % Although tree_to_list/1 requires inst strict (there's no mode
        % declaration for other insts), the compiler allows the call.
        % This by the way has really bad side-effects. Apparently subsequent
        % calls to next2(P) omit the lazy parts of the tree.
        % foldl((pred(decomp(C, _R)::in, !.SubIO::di, !:SubIO::uo) is det :-
        %     Fmt("    *** %s\n", [s(if C = primf(AF) then string(AF(S)) else string(C))], !SubIO)
        % ), reverse(tree_to_list(next2(P))), !.IO, !:IO)
        % XXX You should investigate on this. Maybe the mode declarations
        % aren't that definitive?
        true
    ;
        Phase = finished,
        Fmt("Finished: program final and covered\n", [], !IO),
        Fmt("    Reward: %s\n", [s(string(reward(S)))], !IO),
        Fmt("    Remaining program:\n", [], !IO),
        Fmt("        %s\n", [s(string(P))], !IO),
        Fmt("    Situation:\n", [], !IO),
        foldl((pred(A::in, !.SubIO::di, !:SubIO::uo) is det :-
            Fmt("        %s\n", [s(string(A))], !SubIO)
        ), reverse(sit2list(S)), !.IO, !:IO),
        debug_conf(P, S, !IO)
    ;
        Phase = failed,
        Fmt("Failure\n", [], !IO),
        %PX = subst_obs(nil, P), ( if final(PX, S) then write_string("Final: ", !IO), write(PX, !IO), nl(!IO) else write_string("Not final: ", !IO), write(PX, !IO), nl(!IO) ),
        %( if last_action_covered_by_obs(S) then format("last action covered by observation\n", [], !IO) else format("last action covered by observation\n", [], !IO) ),
        Fmt("    Reward: %s\n", [s(string(reward(S)))], !IO),
        Fmt("    Remaining program:\n", [], !IO),
        Fmt("        %s\n", [s(string(P))], !IO),
        Fmt("    Situation:\n", [], !IO),
        foldl((pred(A::in, !.SubIO::di, !:SubIO::uo) is det :-
            Fmt("        %s\n", [s(string(A))], !SubIO)
        ), reverse(sit2list(S)), !.IO, !:IO),
        debug_conf(P, S, !IO)
    ),
    (
        if      Phase = finishing
        then    Fmt("All observations received, finishing\n", [], !IO)
        else    true
    ),
    Fmt("    start = %s\n", [s(if Start = start(S) then string(Start) else "undef")], !IO),
    %some [NTG] ( if NTG = ntg(d,h,S) then Fmt("    ntg(d,h) = %s\n", [s(string(NTG))], !IO) else true ),
    some [NTG] ( if NTG = ntg(h,b,S) then Fmt("    ntg(h,b) = %s\n", [s(string(NTG))], !IO) else true ),
    some [NTG] ( if NTG = ntg(h,d,S) then Fmt("    ntg(h,d) = %s\n", [s(string(NTG))], !IO) else true ),
    some [NTG] ( if NTG = ntg(b,d,S) then Fmt("    ntg(b,d) = %s\n", [s(string(NTG))], !IO) else true ),
    %some [TTC] ( if TTC = ttc(d,h,S) then Fmt("    ttc(d,h) = %s\n", [s(string(TTC))], !IO) else true ),
    some [TTC] ( if TTC = ttc(h,b,S) then Fmt("    ttc(h,b) = %s\n", [s(string(TTC))], !IO) else true ),
    some [TTC] ( if TTC = ttc(h,d,S) then Fmt("    ttc(h,d) = %s\n", [s(string(TTC))], !IO) else true ),
    some [TTC] ( if TTC = ttc(b,d,S) then Fmt("    ttc(b,d) = %s\n", [s(string(TTC))], !IO) else true ),
    some [Lane] ( if Lane = lane(b,S) then Fmt("    lane(b) = %s\n", [s(string(Lane))], !IO) else true ),
    some [Lane] ( if Lane = lane(d,S) then Fmt("    lane(d) = %s\n", [s(string(Lane))], !IO) else true ),
    some [Lane] ( if Lane = lane(h,S) then Fmt("    lane(h) = %s\n", [s(string(Lane))], !IO) else true ),
    % Progression is currently broken because sitlen/1 is not not handled as
    % fluent and thus breaks the reward computation.
    S1 = S, %( if fail, sitlen(S) > 6 then format("Progressing S!!!\n", [], !IO), progress(S, S1, !IO) else S1 = S ),
    true.

%-----------------------------------------------------------------------------%

:- pred accept_connections(int::in, visual.areas::in,
                           io::di, io::uo) is cc_multi.

accept_connections(ServerSocket, Areas, !IO) :-
    % XXX number samples!
    % reasonable value for dual core (one free core @ 2.2 GHz): 9
    % reasonable value for core i7 (four free cores @ 3.2 GHz): 27
    NSamples = 1,
    %Handler = visual.visualize(Areas),
    Progs = %[ tailgate(h, d) ] ++
            %[ follow(h, d) ] ++
            %[ overtake(h, d) ] ++
            %[ pass(d, b) ] ++
            [ pass(d, b) // (approach(h, b) `;` overtake(h, b)) ] ++
            [] `with_type` list(rstc.prog(float)),
    times(StartTime, !IO),
    foldl4((pred(Prog::in,
                 N::in, N+1::out,
                 Sources1::in, [Source|Sources1]::out,
                 Varss1::in, [Vars|Varss1]::out,
                 !.SubIO::di, !:SubIO::uo) is cc_multi :-
        new_source(Source, !SubIO),
        Handler = stdout_handler(Source, N) `with_type` handler(rstc.prim(_)),% `with_inst` handler,
        online_planrecog(NSamples, Source, Vars, Handler, Prog, !SubIO)
    ), Progs, 1, _, [], Sources, [], Varss, !IO),
    accept_connection(ServerSocket, Socket, !IO),
    handle_connection(Socket, !IO),
    foldl_corresponding((pred(Source::in, Vars::in, !.SubIO::di, !:SubIO::uo) is det :-
        wait_for_planrecog_finish(Source, Vars, !SubIO) % might have bad consequences (post all semaphores)
    ), Sources, Varss, !IO),
    finalize_connection(Socket, !IO),
    reset_all_sources(!IO),
    times(EndTime, !IO),
    format("Usertime: %f\n", [f(usertime(StartTime, EndTime))], !IO),
    format("Systime: %f\n", [f(systime(StartTime, EndTime))], !IO),
    %rstc.print_memo_stats(!IO),
    %bat.print_memo_stats(!IO),
    rstc.reset_memo(!IO),
    bat.reset_memo(!IO),
    accept_connections(ServerSocket, Areas, !IO).

%-----------------------------------------------------------------------------%

main(!IO) :-
    make_server_socket(ServerSocket, !IO),
    Areas=[],% visual.init(9, Areas, !IO),
    accept_connections(ServerSocket, Areas, !IO),
    %visual.finish(!IO),
    true.

%-----------------------------------------------------------------------------%
:- end_module cars_server.
%-----------------------------------------------------------------------------%

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
:- import_module domain.car.cont.
:- import_module domain.car.obs.
:- import_module domain.car.obs.torcs.
:- import_module list.
:- import_module planrecog.
:- import_module prgolog.
:- import_module prgolog.nice.
:- import_module string.
:- import_module visual.

%-----------------------------------------------------------------------------%

:- func confidence = (float::out) is det.

:- pragma foreign_proc("C",
    confidence = (Conf::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Conf = (MR_Float) confidence();
").


:- pragma foreign_decl("C", "
    #include <assert.h>
    #include <netinet/in.h>
    #include <string.h>
    #include <stdio.h>
    #include <strings.h>
    #include <sys/types.h>
    #include <sys/socket.h>
    #include <unistd.h>

    #define DEBUG printf
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
        push_obs(&obs);

//      printf(""%c\\t%10.5lf\\t"", 'O', obs.t);
//      printf(""%5s\\t%10.5lf\\t%10.5lf\\t%10.5lf\\t%10.5lf"", obs.agent0, obs.veloc0, obs.rad0, obs.x0, obs.y0);
//      printf(""%5s\\t%10.5lf\\t%10.5lf\\t%10.5lf\\t%10.5lf"", obs.agent1, obs.veloc1, obs.rad1, obs.x1, obs.y1);
//      printf(""\\n"");

//      DEBUG(""read observation %.2lf  (%s: %.2lf %.2lf %.2lf %.2lf; ""
//                                     ""%s: %.2lf %.2lf %.2lf %.2lf)\\n"",
//            obs.t, obs.agent0, obs.veloc0, obs.rad0, obs.x0, obs.y0,
//                   obs.agent1, obs.veloc1, obs.rad1, obs.x1, obs.y1);

        init_message(&msg);
        ret = write(Socket, &msg, sizeof(msg));
        if (ret != sizeof(msg)) {
            break;
        }
//      DEBUG(""write confidence %f\\n"", confidence());
    }
    IO1 = IO0;
").


:- pred accept_connections(int::in, io::di, io::uo) is cc_multi.

accept_connections(ServerSocket, !IO) :-
    % XXX number samples!
    % reasonable value for dual core (one free core @ 2.2 GHz): 9
    % reasonable value for core i7 (four free cores @ 3.2 GHz): 27
    NSamples = 9,
    Prog = (p(cruise(a)) // p(overtake(b, a))) `with_type` prog(prim, stoch, proc),
    accept_connection(ServerSocket, Socket, !IO),
    Source = source,
    reset_obs_source(Source, !IO),
    init_visual(6, Areas, !IO),
    online_planrecog(NSamples, Source, Vars, visualize(Areas), Prog, !IO),
    handle_connection(Socket, !IO),
    %format("Connection terminated, waiting for plan recognition...\n", [], !IO),
    wait_for_planrecog_finish(Source, Vars, !IO),
    %wait_for_key(!IO),
    finish_visual(!IO),
    %format("Plan recognition finished with confidence %.2f.\n",
    %       [f(confidence)], !IO),
    finalize_connection(Socket, !IO),
    accept_connections(ServerSocket, !IO),
    true.% ( if Cont = yes then accept_connections(ServerSocket, !IO) else true ).


main(!IO) :-
    make_server_socket(ServerSocket, !IO),
    accept_connections(ServerSocket, !IO).

%-----------------------------------------------------------------------------%
:- end_module cars_server.
%-----------------------------------------------------------------------------%

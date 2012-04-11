%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
%
% File: prs.m.
% Main author: schwering.
%
% Christoph Schwering (schwering@kbsg.rwth-aachen.de)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module prs.

:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module obs.
:- import_module planrecog.
:- import_module string.

%-----------------------------------------------------------------------------%

:- pragma foreign_code("C", "
    static double confidence(void) {
        int i;
        double c = 0.0;
        int n = 0;
        for (i = 0; i < NSAMPLES; ++i) {
            if (states[i].activity == WORKING || states[i].activity == FINISHED) {
                if (states[i].done + states[i].tbd != 0) {
                    c += (double) states[i].done / (double) (states[i].done + states[i].tbd);
                }
                ++n;
            }
        }
        return (n > 0) ? c / (double) n : 0.0;
    }
").


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

    #define PORT 19123
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
        struct record obs;
        float conf;
        int ret;
        ret = read(Socket, &obs, sizeof(obs));
        if (ret != sizeof(obs)) {
            break;
        }
        push_obs(&obs);
//      DEBUG(""read observation %.2lf  (%s: %.2lf %.2lf %.2lf %.2lf; ""
//                                     ""%s: %.2lf %.2lf %.2lf %.2lf)\\n"",
//            obs.t, obs.agent0, obs.veloc0, obs.rad0, obs.x0, obs.y0,
//                   obs.agent1, obs.veloc1, obs.rad1, obs.x1, obs.y1);
        conf = confidence();
        ret = write(Socket, &conf, sizeof(conf));
        if (ret != sizeof(conf)) {
            break;
        }
        DEBUG(""write confidence %lf\\n"", confidence());
    }
    IO1 = IO0;
").


:- pred accept_connections(int::in, io::di, io::uo) is cc_multi.

accept_connections(ServerSocket, !IO) :-
    accept_connection(ServerSocket, Socket, !IO),
    online_planrecog(10, Vars, !IO),
    handle_connection(Socket, !IO),
    format("Connection terminated, waiting for plan recognition...\n", [], !IO),
    wait_for_planrecog_finish(Vars, !IO),
    format("Plan recognition finished with confidence %.2f.\n",
           [f(confidence)], !IO),
    finalize_connection(Socket, !IO),
    accept_connections(ServerSocket, !IO).


main(!IO) :-
    make_server_socket(ServerSocket, !IO),
    accept_connections(ServerSocket, !IO).

%-----------------------------------------------------------------------------%
:- end_module prs.
%-----------------------------------------------------------------------------%

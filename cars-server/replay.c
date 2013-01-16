/* vim: ft=c ts=4 sw=4 et wm=0 tw=0
 * Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 *
 * File: replay.c
 * Main author: schwering.
 *
 * Simulates a TORCS instance by emitting observations read from stdin or a
 * file. It cares about the time periods between observations.
 */

#include <netdb.h>
#include <netinet/in.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

#include <domain-car-obs-torcs-types.h>

#define HOST "localhost"
#define PORT 19123

static int make_socket(void)
{
    int sockfd;
    struct sockaddr_in server_addr;
    struct hostent *server;

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        fprintf(stderr, "Couldn't open socket\n");
        exit(1);
    }
    server = gethostbyname(HOST);
    if (server == NULL) {
        fprintf(stderr, "Couldn't resolve host %s\n", HOST);
        exit(1);
    }
    bzero((char *) &server_addr, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    bcopy( server->h_addr, &server_addr.sin_addr.s_addr, server->h_length);
    server_addr.sin_port = htons(PORT);
    if (connect(sockfd, (struct sockaddr *) &server_addr, sizeof(server_addr)) < 0) {
        fprintf(stderr, "Couldn't connect to server\n");
        exit(1);
    }
    return sockfd;
}

static float min_conf(const struct planrecog_state *msg, const int i)
{
    const int numer = msg->sources[i].finished;
    const int denom = msg->sources[i].working + msg->sources[i].finished +
                      msg->sources[i].failed;
    return (double) numer / (double) denom;
}

static float max_conf(const struct planrecog_state *msg, const int i)
{
    const int numer = msg->sources[i].working + msg->sources[i].finished;
    const int denom = msg->sources[i].working + msg->sources[i].finished +
                      msg->sources[i].failed;
    return (double) numer / (double) denom;
}

static void klatschtgleich2(FILE *fp, int sockfd, bool do_sleep)
{
    struct observation_record r;
    struct planrecog_state state;
    double t0 = -1.0;

    while (fscanf(fp, "%*c %lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\n",
                &r.t,
                r.info[0].agent, &r.info[0].veloc, &r.info[0].rad, &r.info[0].x, &r.info[0].y,
                r.info[1].agent, &r.info[1].veloc, &r.info[1].rad, &r.info[1].x, &r.info[1].y) == 11) {
        int i, ret;

        r.n_agents = 2;

        printf("%lf", r.t);
        for (i = 0; i < r.n_agents; ++i) {
            printf(" %lf '%s' %lf %lf %lf %lf",
                   r.info[i].agent, r.info[i].veloc, r.info[i].rad,
                   r.info[i].x, r.info[i].y);
        }
        printf("\n");

        if (do_sleep && t0 >= 0.0) {
            usleep((useconds_t) (1e6 * (r.t - t0)));
        }
        t0 = r.t;

        ret = write(sockfd, &r, sizeof(r));
        if (ret != sizeof(r)) {
            fprintf(stderr, "Couldn't write %lu bytes\n", sizeof(r));
            exit(1);
        }

        ret = read(sockfd, &state, sizeof(state));
        if (ret != sizeof(state)) {
            fprintf(stderr, "Couldn't read %lu bytes\n", sizeof(state));
            exit(1);
        }

        for (i = 0; i < r.n_agents; ++i) {
            int j;
            for (j = 0; j < r.n_agents; ++j) {
                if (i != j) {
                    const double ntg = (r.info[j].x - r.info[i].x) / r.info[i].veloc;
                    const double ttc = (r.info[j].x - r.info[i].x) / (r.info[i].veloc - r.info[j].veloc);
                    printf("ntg('%s', '%s') = %7.2lf\t\t", r.info[i].agent, r.info[j].agent, ntg);
                    printf("ttc('%s', '%s') = %7.2lf\n", r.info[i].agent, r.info[j].agent, ttc);
                }
            }
        }
        printf("%.2lf =< confidence working=%d, finished=%d, failed=%d =< %.2lf\n",
                min_conf(&state, 0), state.sources[0].working, state.sources[0].finished, state.sources[0].failed, max_conf(&state, 0));
    }
}

int main(int argc, char *argv[])
{
    int sockfd = make_socket();
    bool do_sleep = true;

    if (argc >= 2 &&
            (!strcmp(argv[1], "-n") ||
             !strcmp(argv[1], "--no-sleep"))) {
        /* XXX Leads to crash for some reason (don't know why right now). */
        do_sleep = false;
    }

    if (argc <= 1) {
        klatschtgleich2(stdin, sockfd, do_sleep);
    } else {
        int i;

        for (i = 1; i < argc; ++i) {
            FILE *fp = fopen(argv[i], "r");
            klatschtgleich2(fp, sockfd, do_sleep);
            fclose(fp);
        }
    }
    close(sockfd);
    return 0;
}


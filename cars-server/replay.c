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

#include <car-obs-torcs-types.h>

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

static float min_conf(const struct planrecog_state *msg)
{
    return (double) (msg->finished) /
           (double) (msg->working + msg->finished + msg->working);
}

static float max_conf(const struct planrecog_state *msg)
{
    return (double) (msg->working + msg->finished) /
           (double) (msg->working + msg->finished + msg->working);
}

static void klatschtgleich2(FILE *fp, int sockfd)
{
    struct observation_record r;
    struct planrecog_state state;
    double t0 = -1.0;
    int ret;

    while (fscanf(fp, "%*c %lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\n",
                &r.t,
                r.info[0].agent, &r.info[0].veloc, &r.info[0].rad, &r.info[0].x, &r.info[0].y,
                r.info[1].agent, &r.info[1].veloc, &r.info[1].rad, &r.info[1].x, &r.info[1].y) == 11) {
        printf("%lf '%s' %lf %lf %lf %lf '%s' %lf %lf %lf %lf\n",
               r.t,
               r.info[0].agent, r.info[0].veloc, r.info[0].rad, r.info[0].x, r.info[0].y,
               r.info[1].agent, r.info[1].veloc, r.info[1].rad, r.info[1].x, r.info[1].y);
        r.n_agents = 2;
        if (t0 >= 0.0) {
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
        printf("%.2lf =< confidence =< %.2lf\n",
                min_conf(&state), max_conf(&state));
    }
}

static void klatschtgleich3(FILE *fp, int sockfd)
{
    struct planrecog_state state;
    double t0 = -1.0;
    int ret;

    for (;;) {
        struct observation_record r;
        int i;

        if (fscanf(fp, "%lf {\n", &r.t) != 1) {
            break;
        }
        printf("%lf {\n", r.t);
        for (i = 0; i < NAGENTS; ++i) {
            struct agent_info_record *info = &r.info[i];
            const int n = fscanf(fp, " %s %lf %lf %lf %lf\n", info->agent,
                    &info->veloc, &info->rad, &info->x, &info->y);
            if (n != 5) {
                if (!(n == 1 && strcmp(info->agent, "}"))) {
                    fprintf(stderr, "Input format is invalid "
                            "(%d elements in line).", n);
                }
                break;
            }
            printf("    %s %lf %lf %lf %lf\n", info->agent, info->veloc,
                    info->rad, info->x, info->y);
        }
        r.n_agents = i;

        if (t0 >= 0.0) {
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
        printf("%.2lf =< confidence =< %.2lf\n",
                min_conf(&state), max_conf(&state));
    }
}

int main(int argc, char *argv[])
{
    int sockfd = make_socket();
    if (argc <= 1) {
        klatschtgleich2(stdin, sockfd);
    } else {
        int i;

        for (i = 1; i < argc; ++i) {
            FILE *fp = fopen(argv[i], "r");
            klatschtgleich2(fp, sockfd);
            fclose(fp);
        }
    }
    close(sockfd);
    return 0;
}


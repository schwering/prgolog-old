/* vim: ft=c ts=4 sw=4 et wm=0 tw=0
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

#include <obs_types.h>

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

static void klatschtgleich2(FILE *fp, int sockfd)
{
    struct record r;
    float conf;
    double t0 = -1.0;
    int ret;

    while (fscanf(fp, "%*c %lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\n",
                &r.t,
                r.agent0, &r.veloc0, &r.rad0, &r.x0, &r.y0,
                r.agent1, &r.veloc1, &r.rad1, &r.x1, &r.y1) == 11) {
        printf("%lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\n",
               r.t,
               r.agent0, r.veloc0, r.rad0, r.x0, r.y0,
               r.agent1, r.veloc1, r.rad1, r.x1, r.y1);
        if (t0 >= 0.0) {
            usleep((useconds_t) (1e6 * (r.t - t0)));
        }
        t0 = r.t;
        ret = write(sockfd, &r, sizeof(r));
        if (ret != sizeof(r)) {
            fprintf(stderr, "Couldn't read %lu bytes\n", sizeof(r));
            exit(1);
        }
        ret = read(sockfd, &conf, sizeof(conf));
        if (ret != sizeof(conf)) {
            fprintf(stderr, "Couldn't read %lu bytes\n", sizeof(conf));
            exit(1);
        }
        printf("confidence %.2lf\n", conf);
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

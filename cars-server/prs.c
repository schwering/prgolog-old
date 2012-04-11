#include <assert.h>
#include <netinet/in.h>
#include <string.h>
#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

#include "mercury_interface.h"

#include <obs.mh>
#include <planrecog.mh>

#define PORT 19123

#define DEBUG printf

static void start_plan_recognition(void)
{
  c_planrecog(5);
  printf("started plan recognition\n");
}

static double confidence(void)
{
  int i;
  double r = 0.0;
  for (i = 0; i < NSAMPLES; ++i) {
    if (states[i].tbd != 0) {
      r += (double) states[i].done / (double) states[i].tbd;
    }
  }
  return r;
}

static int make_server_socket(void)
{
  int sockfd;
  struct sockaddr_in server_addr;

  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
    fprintf(stderr, "Couldn't open socket\n");
    exit(1);
  }
  bzero((char*) &server_addr, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  server_addr.sin_addr.s_addr = INADDR_ANY;
  server_addr.sin_port = htons(PORT);
  if (bind(sockfd, (struct sockaddr*) &server_addr, sizeof(server_addr)) < 0) {
    fprintf(stderr, "Couldn't bind socket\n");
    exit(1);
  }
  listen(sockfd, 1);
  return sockfd;
}

static void session(int sockfd)
{
  start_plan_recognition();
  for (;;) {
    struct record obs;
    float conf;
    read(sockfd, &obs, sizeof(obs));
    push_obs(&obs);
    DEBUG("read observation %.2lf  (%s: %.2lf %.2lf %.2lf %.2lf; "
                                   "%s: %.2lf %.2lf %.2lf %.2lf)\n",
          obs.t, obs.agent0, obs.veloc0, obs.rad0, obs.x0, obs.y0,
                 obs.agent1, obs.veloc1, obs.rad1, obs.x1, obs.y1);
    conf = confidence();
    write(sockfd, &conf, sizeof(conf));
    DEBUG("write confidence %lf\n", conf);
  }
}

int main(int argc, char *argv[])
{
  void* stackbottom;
  int sockfd;

  mercury_init(argc, argv, &stackbottom);

  sockfd = make_server_socket();
  for (;;) {
    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);
    int newsockfd = accept(sockfd, (struct sockaddr*) &client_addr, &client_len);
    if (newsockfd < 0) {
      fprintf(stderr, "Couldn't accept connection\n");
      exit(1);
    }
    session(newsockfd);
    close(newsockfd);
  }
  close(sockfd);

  mercury_terminate();

  return 0;
}


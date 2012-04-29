/*
 * File: lp-sock.h.
 * Main author: schwering.
 *
 * Simple abstraction layer for sockets.
 * Provides functions for AF_UNIX and AF_INET sockets as we use them for the LP
 * server and client.
 * All functions are inline; there's no associated object file.
 *
 * Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 */

#ifndef _LP_SOCK_H_
#define _LP_SOCK_H_


#define UNIX_SOCKETS
#define UNIX_SOCKET_PATH "/tmp/lp_socket"


#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

static inline int connect_unix_socket(const char *path)
{
  const int sockfd = socket(AF_UNIX, SOCK_STREAM, 0);
  struct sockaddr_un server_addr;
  size_t len;

  server_addr.sun_family = AF_UNIX;
  strcpy(server_addr.sun_path, path);
  len = sizeof(server_addr.sun_family) + strlen(server_addr.sun_path);

  if (connect(sockfd, (struct sockaddr *) &server_addr, len) < 0) {
    fprintf(stderr, "Couldn't connect to server\n");
    exit(1);
  }
  return sockfd;
}

static inline int connect_tcp_socket(const char *host, short port)
{
  const int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in server_addr;
  struct hostent *server;

  if (sockfd < 0) {
    fprintf(stderr, "Couldn't open socket\n");
    exit(1);
  }
  server = gethostbyname(host);
  if (server == NULL) {
    fprintf(stderr, "Couldn't resolve host %s\n", host);
    exit(1);
  }
  bzero((char *) &server_addr, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  bcopy( server->h_addr, &server_addr.sin_addr.s_addr, server->h_length);
  server_addr.sin_port = htons(port);

  if (connect(sockfd, (struct sockaddr *) &server_addr,
              sizeof(server_addr)) < 0) {
    fprintf(stderr, "Couldn't connect to server\n");
    exit(1);
  }
  return sockfd;
}


static inline int listen_unix_socket(const char *path)
{
  const int sockfd = socket(AF_UNIX, SOCK_STREAM, 0);
  struct sockaddr_un server_addr;
  size_t len;

  if (socket < 0) {
      fprintf(stderr, "Couldn't open socket\n");
      exit(1);
  }
  server_addr.sun_family = AF_UNIX;
  strcpy(server_addr.sun_path, path);
  unlink(server_addr.sun_path);
  len = sizeof(server_addr.sun_family) + strlen(server_addr.sun_path);

  if (bind(sockfd, (struct sockaddr*) &server_addr, len) < 0) {
      fprintf(stderr, "Couldn't bind socket\n");
      exit(1);
  }
  listen(sockfd, 5);
  return sockfd;
}

static inline int listen_tcp_socket(uint16_t port)
{
  const int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in server_addr;

  if (socket < 0) {
      fprintf(stderr, "Couldn't open socket\n");
      exit(1);
  }
  bzero((char*) &server_addr, sizeof(server_addr));
  server_addr.sin_family = AF_INET;
  server_addr.sin_addr.s_addr = INADDR_ANY;
  server_addr.sin_port = htons(port);

  if (bind(sockfd, (struct sockaddr*) &server_addr, sizeof(server_addr)) < 0) {
      fprintf(stderr, "Couldn't bind socket\n");
      exit(1);
  }
  listen(sockfd, 5);
  return sockfd;
}

static inline int accept_connection(int server_sockfd)
{
  const int sockfd = accept(server_sockfd, NULL, NULL);
  if (sockfd < 0) {
    fprintf(stderr, "Couldn't accept connection\n");
    exit(1);
  }
  return sockfd;
}

#endif


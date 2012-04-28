/*
 * File: lps.cc
 * Main author: schwering.
 *
 * Linear systems solver server.
 *
 * Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 */

#include <assert.h>
#include <netinet/in.h>
#include <string.h>
#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "lp-msg.h"
#include "coin-clp.h"

//#define LOG               printf
#define LOG(fmt, ...)

static int make_server_socket(uint16_t port)
{
  struct sockaddr_in server_addr;
  int sockfd;

  sockfd = socket(AF_INET, SOCK_STREAM, 0);
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


static int accept_connection(int server_sockfd)
{
  int sockfd;
  struct sockaddr_in client_addr;
  socklen_t client_len = sizeof(client_addr);
  sockfd = accept(server_sockfd, (struct sockaddr*) &client_addr, &client_len);
  if (sockfd < 0) {
    fprintf(stderr, "Couldn't accept connection\n");
    exit(1);
  }
  return sockfd;
}


static void answer_solution(SolverContext& state, int sockfd)
{
  Header h;
  void* payload;

  h.type = MSG_SUCCESS;
  h.len = state.varcnt();
  payload = alloc_payload(&h);

  LOG("solving ...\n");
  bool succ = state.solve(&static_cast<Solution*>(payload)->val,
                          static_cast<Solution*>(payload)->as);
  LOG("solved (%s)\n", succ ? "successfully" : "unsuccessfully");
  if (!succ) {
    free_payload(&h, payload);
    h.type = MSG_FAILURE;
    h.len = 0;
    payload = alloc_payload(&h);
  }

  LOG("sending answer %d / %d\n", (int) h.type, (int) h.len);
  if (!send_msg(sockfd, &h, payload)) {
    free_payload(&h, payload);
    fprintf(stderr, "%s:%d: Couldn't write header\n", __FILE__, __LINE__);
    exit(1);
  }
  LOG("sent answer %d / %d\n", (int) h.type, (int) h.len);
  free_payload(&h, payload);
}


static void handle_msg(SolverContext*& state,
                           int sockfd,
                           const Header* h,
                           const void* payload)
{
  LOG("message handling starts\n");
  switch (h->type) {
    case MSG_INIT: {
      LOG("received MSG_INIT %d\n", (int) h->len);
      if (state != NULL)
        delete state;
      state = new SolverContext(h->len);
      break;
    }
    case MSG_CSTR: {
      LOG("received MSG_CSTR %d\n", (int) h->len);
      assert(state != NULL);
      const Constraint* c = static_cast<const Constraint*>(payload);
      state->add_constraint(h->len, c->as, c->vs, c->cmp, c->bnd);
      break;
    }
    case MSG_SOLVE: {
      LOG("received MSG_SOLVE %d\n", (int) h->len);
      assert(state != NULL);
      answer_solution(*state, sockfd);
      break;
    }
    case MSG_SUCCESS:
      fprintf(stderr, "%s:%d: Cannot handle MSG_SUCCESS\n", __FILE__, __LINE__);
      exit(1);
    case MSG_FAILURE:
      fprintf(stderr, "%s:%d: Cannot handle MSG_FAILURE\n", __FILE__, __LINE__);
      exit(1);
    default:
      fprintf(stderr, "%s:%d: Invalid message type\n", __FILE__, __LINE__);
      exit(1);
  }
  LOG("message handling finished\n");
}


static void* session(void* args)
{
  int sockfd = *((int*) args);
  SolverContext* state = NULL;
  LOG("starting session\n");
  for (;;) {
    Header h;
    void* payload;
    LOG("receiving\n");
    if (!recv_msg(sockfd, &h, &payload)) {
      fprintf(stderr, "%s:%d: Couldn't read message\n", __FILE__, __LINE__);
      exit(1);
    }
    LOG("received\n");
    handle_msg(state, sockfd, &h, payload);
    free_payload(&h, payload);
  }
  if (state != NULL)
    delete state;
  close(sockfd);
}


int main(int argc, char* argv[])
{
  int server_sockfd = make_server_socket(LP_PORT);
  for (;;) {
    int sockfd = accept_connection(server_sockfd);
    LOG("accepted connection\n");
    pthread_t thread;
    pthread_create(&thread, NULL, &session, &sockfd);
  }
  close(server_sockfd);
  return 0;
}


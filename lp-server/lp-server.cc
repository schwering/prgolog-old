/*
 * Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 *
 * File: lps.cc
 * Main author: schwering.
 *
 * Linear systems solver server.
 */

#include <string.h>
#include <stdio.h>
#include <strings.h>

#include "lp-msg.h"
#include "lp-sock.h"
#include "coin-clp.h"

//#define LOG               printf
#define LOG(fmt, ...)

#define NCPUS 2

static void answer_solution(SolverContext& state, int sockfd)
{
  Header h;
  void* payload;

  h.type = MSG_SUCCESS;
  h.len = state.varcnt();
  payload = alloc_payload(&h);

  LOG("%d: solving ...\n", sockfd);
  bool succ = state.solve(&static_cast<Solution*>(payload)->val,
                          static_cast<Solution*>(payload)->as);
  LOG("%d: solved (%s)\n", sockfd, succ ? "successfully" : "unsuccessfully");
  if (!succ) {
    free_payload(&h, payload);
    h.type = MSG_FAILURE;
    h.len = 0;
    payload = alloc_payload(&h);
  }

  LOG("%d: sending answer %d / %d\n", sockfd, (int) h.type, (int) h.len);
  if (!send_msg(sockfd, &h, payload)) {
    free_payload(&h, payload);
    fprintf(stderr, "%s:%d: Couldn't write header\n", __FILE__, __LINE__);
    exit(1);
  }
  LOG("%d: sent answer %d / %d\n", sockfd, (int) h.type, (int) h.len);
  free_payload(&h, payload);
}


static void handle_msg(SolverContext*& state,
                       int sockfd,
                       const Header* h,
                       const void* payload)
{
  LOG("%d: message handling starts\n", sockfd);
  switch (h->type) {
    case MSG_INIT: {
      LOG("%d: received MSG_INIT %d\n", sockfd, (int) h->len);
      if (state != NULL) {
        delete state;
        state = NULL;
      }
      state = new SolverContext(h->len);
      break;
    }
    case MSG_CSTR: {
      LOG("%d: received MSG_CSTR %d\n", sockfd, (int) h->len);
      assert(state != NULL);
      const Constraint* c = static_cast<const Constraint*>(payload);
      state->add_constraint(h->len, c->as, c->vs, c->cmp, c->bnd);
      break;
    }
    case MSG_SOLVE: {
      LOG("%d: received MSG_SOLVE %d\n", sockfd, (int) h->len);
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
  LOG("%d: message handling finished\n", sockfd);
}


static void* session(void* args)
{
  int sockfd = *((int*) args);
  free(args); /* Allocated by thread creator. */
  SolverContext* state = NULL;
  LOG("%d: starting session\n", sockfd);
  for (;;) {
    Header h;
    void* payload;
    LOG("%d: receiving\n", sockfd);
    if (!recv_msg(sockfd, &h, &payload)) {
      fprintf(stderr, "%s:%d: Couldn't read message, finishing session\n",
              __FILE__, __LINE__);
      break;
    }
    LOG("%d: received\n", sockfd);
    handle_msg(state, sockfd, &h, payload);
    free_payload(&h, payload);
  }
  if (state != NULL)
    delete state;
  close(sockfd);
  return NULL;
}


int main(int argc, char* argv[])
{
#ifdef UNIX_SOCKETS
  int server_sockfd = listen_unix_socket(UNIX_SOCKET_PATH);
#else
  int server_sockfd = listen_tcp_socket(LP_PORT);
#endif
  for (;;) {
    /* Thread argument must no be on the stack! */
    int* sockfd = (int*) malloc(sizeof(int)); /* De-allocated by thread. */
    *sockfd = accept_connection(server_sockfd);
    LOG("accepted connection %d\n", *sockfd);
    pthread_t thread;
    pthread_create(&thread, NULL, &session, sockfd);
#if 0
    const size_t cpu_set_size = CPU_ALLOC_SIZE(NCPUS);
    cpu_set_t* cpu_set = CPU_ALLOC(NCPUS);
    CPU_ZERO_S(cpu_set_size, cpu_set);
    CPU_SET_S(0, cpu_set_size, cpu_set);
    pthread_setaffinity_np(thread, cpu_set_size, cpu_set);
    CPU_FREE(cpu_set);
#endif
  }
  close(server_sockfd);
  return 0;
}


/*
 * File: lp-msg.h.
 * Main author: schwering.
 *
 * Linear systems solver server.
 *
 * Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 */

#ifndef _LP_MSG_H_
#define _LP_MSG_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "lp-types.h"
#include <arpa/inet.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


#ifndef LP_MALLOC
#define LP_MALLOC  malloc
#define LP_FREE    free
#endif

#define LP_PORT 20123


#define MSG_INIT    1
#define MSG_CSTR    2
#define MSG_SOLVE   3
#define MSG_SUCCESS 4
#define MSG_FAILURE 5

typedef struct {
  msg_type_t type;
  len_t len;
} Header;

typedef struct {
  num_t *as;
  var_t *vs;
  cmp_type_t cmp;
  num_t bnd;
} Constraint;

typedef struct {
  num_t *as;
  num_t val;
} Solution;


/* The size needed for the serialization of a payload. */
static inline size_t size_of_payload(const Header *h)
{
  switch (h->type) {
    case MSG_INIT: return 0;
    case MSG_CSTR: return sizeof(num_t) * h->len +
                          sizeof(var_t) * h->len +
                          sizeof(cmp_type_t) +
                          sizeof(num_t);
    case MSG_SOLVE: return 0;
    case MSG_SUCCESS: return sizeof(num_t) * h->len +
                             sizeof(num_t);
    case MSG_FAILURE: return 0;
    default:
      fprintf(stderr, "%s:%d: Invalid message type %d\n",
              __FILE__, __LINE__, h->type);
      exit(1);
  }
}


#define LP_ALLOC(n, t) (t *) LP_MALLOC((n) * sizeof(t))

#define CHECK_ALLOC(ptr) \
  if (!(ptr)) {\
    fprintf(stderr, "%s:%d: memory allocation failed\n", __FILE__, __LINE__);\
    exit(1);\
  }


/* Allocates a new payload. See free_payload(). */
static inline void *alloc_payload(const Header *h)
{
  switch (h->type) {
    case MSG_INIT: {
      return NULL;
    }
    case MSG_CSTR: {
      Constraint *c = LP_ALLOC(1, Constraint);
      CHECK_ALLOC(c);
      c->as = LP_ALLOC(h->len, num_t);
      CHECK_ALLOC(c->as);
      c->vs = LP_ALLOC(h->len, var_t);
      CHECK_ALLOC(c->vs);
      return c;
    }
    case MSG_SOLVE:
      return NULL;
    case MSG_SUCCESS: {
      Solution *s = LP_ALLOC(1, Solution);
      CHECK_ALLOC(s);
      s->as = LP_ALLOC(h->len, num_t);
      CHECK_ALLOC(s->as);
      return s;
    }
    case MSG_FAILURE: {
      return NULL;
    }
    default:
      fprintf(stderr, "%s:%d: Invalid message type\n", __FILE__, __LINE__);
      exit(1);
  }
}


/* De-allocates a payload. See alloc_payload(). */
static inline void free_payload(const Header *h,
                                void *payload)
{
  if (payload) {
    switch (h->type) {
      case MSG_INIT:
        break;
      case MSG_CSTR:
        LP_FREE(((Constraint *) payload)->vs);
        LP_FREE(((Constraint *) payload)->as);
        break;
      case MSG_SOLVE:
        break;
      case MSG_SUCCESS:
        LP_FREE(((Solution *) payload)->as);
        break;
      case MSG_FAILURE:
        break;
      default:
        fprintf(stderr, "%s:%d: Invalid message type\n", __FILE__, __LINE__);
        exit(1);
    }
    LP_FREE(payload);
  }
}


#define WRITE_TO_BUFFER(buf, off, size, val) \
    if ((off) + sizeof(val) > (size)) {\
      fprintf(stderr, "%s:%d: Buffer too small (%lu + %lu > %lu)\n",\
              __FILE__, __LINE__, (off), sizeof(val), (size));\
    }\
    memcpy((buf) + (offset), &(val), sizeof(val));\
    (off) += sizeof(val)

#define READ_FROM_BUFFER(val, buf, off, size) \
    if ((off) + sizeof(val) > (size)) {\
      fprintf(stderr, "%s:%d: Buffer too small (%lu + %lu > %lu)\n",\
              __FILE__, __LINE__, (off), sizeof(val), (size));\
    }\
    memcpy(&(val), (buf) + (offset), sizeof(val));\
    (off) += sizeof(val)


/* Serializes payload whose metadata is h into buf whose length is at most
 * bufsize (when this is exceeded, an error is thrown).
 * Note: the header h is not written to buf! */
static inline void write_payload_to_buf(void *buf,
                                        size_t bufsize,
                                        const Header *h,
                                        const void *payload)
{
  char *ptr = (char *) buf;
  size_t offset = 0;
  int i;
  switch (h->type) {
    case MSG_INIT: {
      break;
    }
    case MSG_CSTR: {
      Constraint *c = (Constraint *) payload;
      for (i = 0; i < h->len; ++i) {
        WRITE_TO_BUFFER(ptr, offset, bufsize, c->as[i]);
        WRITE_TO_BUFFER(ptr, offset, bufsize, c->vs[i]);
      }
      WRITE_TO_BUFFER(ptr, offset, bufsize, c->cmp);
      WRITE_TO_BUFFER(ptr, offset, bufsize, c->bnd);
      break;
    }
    case MSG_SOLVE: {
      break;
    }
    case MSG_SUCCESS: {
      Solution *s = (Solution *) payload;
      for (i = 0; i < h->len; ++i) {
        WRITE_TO_BUFFER(ptr, offset, bufsize, s->as[i]);
      }
      WRITE_TO_BUFFER(ptr, offset, bufsize, s->val);
      break;
    }
    case MSG_FAILURE: {
      break;
    }
    default:
      fprintf(stderr, "%s:%d: Invalid message type\n", __FILE__, __LINE__);
      exit(1);
  }
}


/* De-serializes the next payload contained in buf whose length is at most
 * bufsize (when more bytes are needed, an error is thrown) into payload.
 * Note: the memory of payload must be allocated by the caller!
 * Note: the header h is not read from buf! */
static inline void read_payload_from_buf(const Header *h,
                                         void *payload,
                                         const void *buf,
                                         size_t bufsize)
{
  const char *ptr = (char *) buf;
  size_t offset = 0;
  int i;
  switch (h->type) {
    case MSG_INIT: {
      break;
    }
    case MSG_CSTR: {
      Constraint *c = (Constraint *) payload;
      for (i = 0; i < h->len; ++i) {
        READ_FROM_BUFFER(c->as[i], ptr, offset, bufsize);
        READ_FROM_BUFFER(c->vs[i], ptr, offset, bufsize);
      }
      READ_FROM_BUFFER(c->cmp, ptr, offset, bufsize);
      READ_FROM_BUFFER(c->bnd, ptr, offset, bufsize);
      break;
    }
    case MSG_SOLVE: {
      break;
    }
    case MSG_SUCCESS: {
      Solution *s = (Solution *) payload;
      for (i = 0; i < h->len; ++i) {
        READ_FROM_BUFFER(s->as[i], ptr, offset, bufsize);
      }
      READ_FROM_BUFFER(s->val, ptr, offset, bufsize);
      break;
    }
    case MSG_FAILURE: {
      break;
    }
    default:
      fprintf(stderr, "%s:%d: Invalid message type\n", __FILE__, __LINE__);
      exit(1);
  }
}


/* Reads the next len bytes to buffer buf from sockfd. */
static inline bool recv_buf(int sockfd, void *buf, size_t len)
{
  char *ptr = (char *) buf;
  size_t done = 0;
  while (done < len) {
    ssize_t s = recv(sockfd, ptr + done, len - done, 0);
    if (s <= 0) {
      break;
    }
    done += s;
  }
  return done == len;
}


/* Sends the next len bytes from buffer buf via sockfd. */
static inline bool send_buf(int sockfd, const void *buf, size_t len)
{
  const char *ptr = (const char *) buf;
  size_t done = 0;
  while (done < len) {
    ssize_t s = send(sockfd, ptr + done, len - done, 0);
    if (s <= 0) {
      break;
    }
    done += s;
  }
  return done == len;
}


/* Receives the next message (header and payload) from sockfd.
 * Note: The message's payload is stored in the newly allocated *payload!
 * The caller needs to de-allocate this with free_payload()! */
static inline bool recv_msg(int sockfd, Header *h, void **payload)
{
  void *payload_buf;
  size_t payload_len;
  if (!recv_buf(sockfd, h, sizeof(*h))) {
    *payload = NULL;
    return false;
  }
  payload_len = size_of_payload(h);
  payload_buf = LP_MALLOC(payload_len);
  if (!recv_buf(sockfd, payload_buf, payload_len)) {
    LP_FREE(payload_buf);
    *payload = NULL;
    return false;
  }
  *payload = alloc_payload(h); /* This one must be freed by the caller! */
  read_payload_from_buf(h, *payload, payload_buf, payload_len);
  LP_FREE(payload_buf);
  return true;
}


/* Sends the next message (header and payload) via sockfd. */
static inline bool send_msg(int sockfd, const Header *h, const void *payload)
{
  const size_t header_len = sizeof(*h);
  const size_t payload_len = size_of_payload(h);
  const size_t total_len = header_len + payload_len;
  void *total_buf = LP_MALLOC(total_len);
  void *payload_buf = (char *) total_buf + header_len;
  memcpy(total_buf, h, header_len);
  write_payload_to_buf(payload_buf, payload_len, h, payload);
  if (!send_buf(sockfd, total_buf, total_len)) {
    LP_FREE(total_buf);
    return false;
  }
  LP_FREE(total_buf);
  return true;
}

#ifdef __cplusplus
}
#endif

#endif


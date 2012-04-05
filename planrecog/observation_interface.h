#ifndef _OBSERVATION_INTERFACE_H_
#define _OBSERVATION_INTERFACE_H_

#include <assert.h>
#include <pthread.h>
#include <stdbool.h>

#define AGENTLEN  15
#define NRECORDS 500
#define NSAMPLES 500

struct record {
  double t;
  char agent0[AGENTLEN+1];
  double veloc0;
  double rad0;
  double x0;
  double y0;
  char agent1[AGENTLEN+1];
  double veloc1;
  double rad1;
  double x1;
  double y1;
};

struct state {
  int done;
  int tbd;
};

extern struct record records[];
extern int max_valid_record;

extern struct state states[];
extern int max_valid_state;

extern pthread_mutex_t mutex;
extern pthread_cond_t cond;
extern volatile bool obs_coming;

static inline void push_obs(const struct record *r)
{
  assert(max_valid_record + 1 < NRECORDS);
  memcpy(&records[max_valid_record + 1], r, sizeof(struct record));
  ++max_valid_record;
  pthread_cond_broadcast(&cond);
}

static inline void read_obs(void)
{
  for (;;) {
    struct record r;
    int i = scanf(
            "%*c %lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\n",
            &r.t,
            r.agent0, &r.veloc0, &r.rad0, &r.x0, &r.y0,
            r.agent1, &r.veloc1, &r.rad1, &r.x1, &r.y1);
    if (i == EOF) {
      obs_coming = false;
      pthread_cond_broadcast(&cond);
      break;
    } else if (i == 11) {
      push_obs(&r);
    }
  }
}

#endif


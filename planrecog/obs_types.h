#ifndef _OBS_TYPES_H_
#define _OBS_TYPES_H_

#define AGENTLEN  15

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

enum activity {
  UNUSED = 0,
  WORKING = 1,
  FINISHED = 2,
  FAILED = 3
};

struct state {
  int done;
  int tbd;
  enum activity activity;
};

struct confidence_pair {
  double online;
  double finished;
};

#endif


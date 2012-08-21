/*
 * Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 *
 * File: obs_types.
 * Main author: schwering.
 *
 * Low-level types for observations from TORCS for the continuous car domain.
 */

#ifndef _OBS_TYPES_H_
#define _OBS_TYPES_H_

#include <stdint.h>

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

struct sample_state {
  int done;
  int tbd;
  enum activity activity;
};

struct state_message {
  uint8_t working;
  uint8_t finished;
  uint8_t failed;
};

#endif


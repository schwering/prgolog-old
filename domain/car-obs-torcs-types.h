/*
 * Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 *
 * File: car-obs-torcs-types.h.
 * Main author: schwering.
 *
 * Low-level types for observations from TORCS for the continuous car domain.
 * It is for one about observations and for another about the processes which
 * represent a sample of the plan recognition system (probabilities are
 * sampled).
 */

#ifndef _OBS_TYPES_H_
#define _OBS_TYPES_H_

#include <stdint.h>

#define PORT 19123

#define AGENTLEN  7

struct agent_info_record {
  char agent[AGENTLEN+1]; /* name of agent */
  double veloc;           /* velocity of agent */
  double rad;             /* yaw of agent */
  double x;               /* longitudinal position of agent */
  double y;               /* lateral position of agent */
};

#define NAGENTS   5

/* Represents an observation of two agents and their physical parameters at a
 * certain point in time. */
struct observation_record {
  double t;       /* timestamp */
  short n_agents; /* number of agents in this observation */
  struct agent_info_record info[NAGENTS]; /* agent info */
};

/* Minor states a sample process can be in. */
enum activity {
  UNUSED = 0,   /* initially the process does nothing */
  WORKING = 1,  /* then the process incrementally executes the program */
  FINISHED = 2, /* finally the process either completes execution */
  FAILED = 3    /* or execution fails */
};

/* State a sample process can be in. */
struct process_state {
  int done;               /* observations matched until now */
  int tbd;                /* observations still to matched */
  enum activity activity; /* is it working, has it succeeded or failed? */
};

/* Global state of the plan recognition system. */
struct planrecog_state {
  uint8_t working;  /* number of processes still working */
  uint8_t finished; /* number of succeeded processes */
  uint8_t failed;   /* number of failed processes */
};

#endif


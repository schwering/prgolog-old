/*
 * Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 *
 * File: lp-types.h.
 * Main author: schwering.
 *
 * Linear systems type definitions.
 */

#ifndef _LP_TYPES_H_
#define _LP_TYPES_H_

#include <stdint.h>

#define CMP_LEQ (-1)
#define CMP_EQ  ( 0)
#define CMP_GEQ ( 1)

typedef int8_t msg_type_t;
typedef int8_t cmp_type_t;
typedef double num_t;
typedef uint16_t var_t;
typedef uint16_t len_t;

#endif


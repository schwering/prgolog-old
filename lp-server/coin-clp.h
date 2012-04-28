/*
 * File: coin-clp.h.
 * Main author: schwering.
 *
 * Wrapper of COIN-OR CLP OSI interface.
 *
 * See coin-clp.cc for details.
 *
 * Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 */

#ifndef _COIN_CLP_H_
#define _COIN_CLP_H_

#include "lp-types.h"
#include <OsiClpSolverInterface.hpp>

class SolverContext {
 public:
  explicit SolverContext(len_t nvars);
  virtual ~SolverContext();

  void add_constraint(len_t n,
                      const num_t* as, const var_t* vs,
                      cmp_type_t cmp, num_t bnd);
  len_t varcnt() const;
  bool solve(num_t* obj_val, num_t* var_vals);

 private:
  SolverContext(const SolverContext&);
  SolverContext& operator=(const SolverContext&);

  len_t nvars;
  OsiClpSolverInterface solver;
  CoinPackedMatrix matrix;
  num_t* var_lb;
  num_t* var_ub;
  num_t* objective;
};

#endif


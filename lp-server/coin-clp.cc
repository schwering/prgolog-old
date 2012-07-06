/*
 * File: coin-clp.cc.
 * Main author: schwering.
 *
 * Wrapper of COIN-OR CLP OSI interface.
 *
 * The header provides a C-interface so that we can use this from Mercury
 * which (to my knowledge) allows interfacing only with C.
 *
 * The body file is written in C++. Then, the body file is compiled with a C++
 * compiler and the object file can be linked by the Mercury compiler.
 *
 * Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 */

#include "coin-clp.h"
#include <cstdio>
#include <cstdlib>
#include <CoinPackedMatrix.hpp>
#include <CoinPackedVector.hpp>

SolverContext::SolverContext(len_t nvars)
  : nvars(nvars),
    matrix(CoinPackedMatrix(false, 0.0, 0.0)),
    var_lb(new num_t[nvars]),
    var_ub(new num_t[nvars]),
    objective(new num_t[nvars])
{
  matrix.setDimensions(0, nvars);
  for (int i = 0; i < nvars; ++i) {
    var_lb[i] = 0.0;
    var_ub[i] = solver.getInfinity();
    objective[i] = 1.0;
  }
  solver.loadProblem(matrix, var_lb, var_ub, objective, NULL, NULL);
  //solver.setHintParam(OsiDoReducePrint);
  solver.messageHandler()->setLogLevel(0); 
}

SolverContext::~SolverContext()
{
  delete[] objective;
  delete[] var_ub;
  delete[] var_lb;
}

void SolverContext::add_constraint(len_t n,
                                   const num_t* as, const var_t* vs,
                                   cmp_type_t cmp, num_t bnd)
{
  try {
    //fprintf(stderr, "n = %d (%d)\n", n, nvars);
    CoinPackedVector row;
    for (int i = 0; i < n; ++i) {
      //fprintf(stderr, "  + %lf * v_%d", as[i], vs[i]);
      assert(vs[i] < nvars);
      row.insert(static_cast<int>(vs[i]), as[i]);
    }
    num_t row_lb, row_ub;
    if (cmp > 0) { // ">="
      row_lb = bnd;
      row_ub = solver.getInfinity();
      //fprintf(stderr, " >= %lf\n", bnd);
    } else if (cmp < 0) { // "=<"
      row_lb = -1.0 * solver.getInfinity();
      row_ub = bnd;
      //fprintf(stderr, " =< %lf\n", bnd);
    } else { // "="
      row_lb = bnd;
      row_ub = bnd;
      //fprintf(stderr, " = %lf\n", bnd);
    }
    solver.addRow(row, row_lb, row_ub);
  } catch (const CoinError& e) {
    fprintf(stderr, "CoinError: %s\n", e.message().c_str());
    for (int i = 0; i < n; ++i) {
      fprintf(stderr, " %d", vs[i]);
    }
    fprintf(stderr, "\n");
  }
}

len_t SolverContext::varcnt() const
{
  return nvars;
}

bool SolverContext::solve(num_t* obj_val, num_t* var_vals)
{
  try {
    solver.initialSolve();
    bool optimal = solver.isProvenOptimal();
    //solver.writeMps("problem");
    if (optimal) {
      if (obj_val) {
        *obj_val = solver.getObjValue();
      }
      if (var_vals) {
        memcpy(var_vals, solver.getColSolution(), nvars * sizeof(num_t));
      }
    } else {
      if (obj_val) {
        *obj_val = 0.0;
      }
    }
    return optimal;
  } catch (const CoinError& e) {
    fprintf(stderr, "CoinError: %s\n", e.message().c_str());
    *obj_val = 0.0;
    return false;
  }
}

SolverContext* new_solver_context(len_t nvars)
{
  return new SolverContext(nvars);
}

void finalize_solver_context(SolverContext** ctx)
{
  if (*ctx) {
    delete *ctx;
  }
}

void add_constraint(SolverContext* ctx, len_t n,
                    const num_t* as, const var_t* vs,
                    cmp_type_t cmp, num_t bnd)
{
  ctx->add_constraint(n, as, vs, cmp, bnd);
}

len_t varcnt(SolverContext* ctx)
{
  return ctx->varcnt();
}

bool solve(SolverContext* ctx, num_t* obj_val, num_t* var_vals)
{
  return ctx->solve(obj_val, var_vals);
}


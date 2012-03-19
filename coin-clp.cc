#include "coin-clp.h"
#include <cstdio>
#include <cstdlib>
#include <CoinPackedMatrix.hpp>
#include <CoinPackedVector.hpp>

SolverContext* new_solver_context(int nvars)
{
  //fprintf(stderr, "init\n");
  SolverContext* ctx = new SolverContext;
  ctx->matrix = new CoinPackedMatrix(false, 0, 0);
  ctx->matrix->setDimensions(0, nvars);
  ctx->var_lb = new double[nvars];
  ctx->var_ub = new double[nvars];
  ctx->objective = new double[nvars];
  ctx->solver = new OsiClpSolverInterface;
  for (int i = 0; i < nvars; ++i) {
    ctx->var_lb[i] = 0.0;
    ctx->var_ub[i] = ctx->solver->getInfinity();
    ctx->objective[i] = 1.0;
  }
  ctx->solver->loadProblem(*ctx->matrix, ctx->var_lb, ctx->var_ub,
                           ctx->objective, 0, 0);
  ctx->nvars = nvars;
  return ctx;
}

void finalize_solver_context(SolverContext* ctx)
{
  delete ctx->solver;
  delete ctx->matrix;
  delete[] ctx->var_lb;
  delete[] ctx->var_ub;
  delete[] ctx->objective;
  delete ctx;
  //fprintf(stderr, "final\n");
}

void add_constraint(SolverContext* ctx, int n, const double* as, const int *vs,
                    int cmp, double bnd)
{
  //fprintf(stderr, "n = %d (%d)\n", n, ctx->nvars);
  CoinPackedVector row;
  for (int i = 0; i < n; ++i) {
    fprintf(stderr, "  + %.1lf * v_%d", as[i], vs[i]);
    row.insert(vs[i], as[i]);
  }
  double row_lb, row_ub;
  if (cmp < 0) { // ">="
    row_lb = bnd;
    row_ub = ctx->solver->getInfinity();
    fprintf(stderr, " >= %.1lf\n", bnd);
  } else if (cmp > 0) { // "=<"
    row_lb = -1.0 * ctx->solver->getInfinity();
    row_lb = bnd;
    fprintf(stderr, " =< %.1lf\n", bnd);
  } else { // "="
    row_lb = bnd;
    row_ub = bnd;
    fprintf(stderr, " = %.1lf\n", bnd);
  }
  ctx->solver->addRow(row, row_lb, row_ub);
}

bool solve(SolverContext* ctx, double* obj_val, double** var_vals, int* nvars)
{
  ctx->solver->initialSolve();
  bool optimal = ctx->solver->isProvenOptimal();
  //ctx->solver->writeMps("problem");
  if (optimal) {
    *obj_val = ctx->solver->getObjValue();
    // use malloc() because Mercury has only free() but no delete
    *var_vals = (double*) malloc(ctx->nvars * sizeof(double));
    memcpy(*var_vals, ctx->solver->getColSolution(),
           ctx->nvars * sizeof(double));
    *nvars = ctx->nvars;
  } else {
    *obj_val = 0.0;
    *var_vals = NULL;
    *nvars = 0;
  }
  finalize_solver_context(ctx);
  return optimal;
}


#ifndef _OSI_C_H_
#define _OSI_C_H_

#ifdef __cplusplus

  #include <OsiClpSolverInterface.hpp>
  typedef struct {
    OsiClpSolverInterface* solver;
    CoinPackedMatrix* matrix;
    int nvars;
    double* var_lb;
    double* var_ub;
    double* objective;
  } SolverContext;

#else

  #include <stdbool.h>
  typedef void SolverContext;

#endif


#ifdef __cplusplus
extern "C" {
#endif

SolverContext* new_solver_context(int nvars);
void finalize_solver_context(SolverContext* ctx);
void add_constraint(SolverContext* ctx, int n, const double* as, const int *vs,
                    int cmp, double bnd);
bool solve(SolverContext* ctx, double* obj_val, double** var_vals,
           int* nvars);

#ifdef __cplusplus
}
#endif

#endif


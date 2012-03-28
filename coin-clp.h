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

#ifdef __cplusplus

  #include <OsiClpSolverInterface.hpp>

  class SolverContext {
   public:
    explicit SolverContext(int nvars);
    virtual ~SolverContext();

    void add_constraint(int n, const double* as, const int* vs,
                        int cmp, double bnd);
    int varcnt() const;
    bool solve(double* obj_val, double* var_vals);

   private:
    SolverContext(const SolverContext&);
    SolverContext& operator=(const SolverContext&);

    int nvars;
    OsiClpSolverInterface solver;
    CoinPackedMatrix matrix;
    double* var_lb;
    double* var_ub;
    double* objective;
  };

#else

  #include <stdbool.h>
  typedef void SolverContext;

#endif


#ifdef __cplusplus
extern "C" {
#endif

SolverContext* new_solver_context(int nvars);
void finalize_solver_context(SolverContext** ctx);
void add_constraint(SolverContext* ctx, int n, const double* as, const int* vs,
                    int cmp, double bnd);
int varcnt(SolverContext* ctx);
bool solve(SolverContext* ctx, double* obj_val, double* var_vals);

#ifdef __cplusplus
}
#endif

#endif


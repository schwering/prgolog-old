/* vim: ft=cpp ts=4 sw=4 et
 */

#include <cstdio>
#include <pthread.h>
extern "C" {
#define GC_THREADS
#define GC_DEBUG
#include <gc.h>
}
#include "coin-clp.h"

#define BLOCK if(1)

static void solve()
{
    SolverContext ctx(26);

#include "blupp"

    double obj_val;
    double* var_values_arr = (double*) GC_MALLOC(ctx.varcnt() * sizeof(double));
    if (solve(&ctx, &obj_val, var_values_arr)) {
        //printf("TID(%d) --> %lf\n", (int) pthread_self(), obj_val);
    } else {
        //printf("TID(%d) --> unsat\n", (int) pthread_self());
    }
    GC_FREE(var_values_arr);

#if 0
    void* ptr = malloc(100);
    free(ptr);
#endif
}

#define NTHREADS 50
#define NLOOPS 100

static void* loop(void*)
{
    for (int i = 0; i < NLOOPS; ++i) {
        GC_MALLOC(100);
        solve();
    }
    return NULL;
}

int main(int argc, char* argv[])
{
    pthread_t threads[NTHREADS];

    GC_INIT();

    for (int i = 0; i < NTHREADS; ++i) {
        //pthread_attr_t attrs;
        //pthread_attr_init(&attrs);
        //pthread_attr_setdetachstate(&attrs, PTHREAD_CREATE_DETACHED);
        pthread_create(&threads[i], NULL, &loop, NULL);
        //pthread_attr_destroy(&attrs);
    }
    for (int i = 0; i < NTHREADS; ++i) {
        //pthread_join(threads[i], NULL);
    }
    //printf("all done\n");
    return 0;
}


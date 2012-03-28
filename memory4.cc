/* vim: ft=cpp ts=4 sw=4 et
 */

#include <cstdio>
#include <pthread.h>
#include "coin-clp.h"
#include <gc.h>

#define BLOCK if(1)

static void solve()
{
    SolverContext ctx(26);

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {24, 25 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 0, 0.000000);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800, -2.174192, 2.174192 };
    int vs_arr[] = {10, 16, 21, 24 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, 1.226059);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800, 2.174192, -2.174192 };
    int vs_arr[] = {10, 16, 21, 24 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, 2.773941);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 20.800000, -0.202424, 0.202424, -0.113945, 0.113945, -5.710000 };
    int vs_arr[] = {3, 5, 10, 16, 21, 24, 25 };
    add_constraint(&ctx, 7, as_arr, vs_arr, 1, -28.472394);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {23, 24 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800, -2.174192, 2.174192 };
    int vs_arr[] = {10, 16, 21, 24 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, 1.226059);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800, 2.174192, -2.174192 };
    int vs_arr[] = {10, 16, 21, 24 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, 2.773941);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {22, 23 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {23 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 12.632000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 23 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -103.741712);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 23 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 108.510272);
    }

    BLOCK {
    double as_arr[] = {20.800000, -0.202424, 0.202424, -0.113945, -20.686055 };
    int vs_arr[] = {5, 10, 16, 21, 23 };
    add_constraint(&ctx, 5, as_arr, vs_arr, 1, -141.498643);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 0.202424, -0.202424, 0.113945, 20.686055 };
    int vs_arr[] = {5, 10, 16, 21, 23 };
    add_constraint(&ctx, 5, as_arr, vs_arr, 1, 150.977303);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800, -2.174192, 2.174192 };
    int vs_arr[] = {10, 16, 21, 23 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, -4.683143);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800, 2.174192, -2.174192 };
    int vs_arr[] = {10, 16, 21, 23 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, 5.097477);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {21, 22 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {22 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 12.132000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 22 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -96.203870);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 22 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 100.972430);
    }

    BLOCK {
    double as_arr[] = {20.800000, -0.202424, 0.202424, -0.113945, -20.686055 };
    int vs_arr[] = {5, 10, 16, 21, 22 };
    add_constraint(&ctx, 5, as_arr, vs_arr, 1, -131.131395);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 0.202424, -0.202424, 0.113945, 20.686055 };
    int vs_arr[] = {5, 10, 16, 21, 22 };
    add_constraint(&ctx, 5, as_arr, vs_arr, 1, 140.610055);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800, -2.174192, 2.174192 };
    int vs_arr[] = {10, 16, 21, 22 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, -5.667070);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800, 2.174192, -2.174192 };
    int vs_arr[] = {10, 16, 21, 22 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, 6.081404);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {20, 21 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800 };
    int vs_arr[] = {10, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -3.773941);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800 };
    int vs_arr[] = {10, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 7.773941);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {19, 20 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {20 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 11.632000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 20 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -88.666028);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 20 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 93.434588);
    }

    BLOCK {
    double as_arr[] = {20.800000, -0.202424, 0.202424, -20.800000 };
    int vs_arr[] = {5, 10, 16, 20 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, -120.690171);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 0.202424, -0.202424, 20.800000 };
    int vs_arr[] = {5, 10, 16, 20 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, 130.168831);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800 };
    int vs_arr[] = {10, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -5.040539);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800 };
    int vs_arr[] = {10, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 7.087745);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {18, 19 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {19 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 11.132000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 19 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -81.128186);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 19 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 85.896746);
    }

    BLOCK {
    double as_arr[] = {20.800000, -0.202424, 0.202424, -20.800000 };
    int vs_arr[] = {5, 10, 16, 19 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, -110.240831);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 0.202424, -0.202424, 20.800000 };
    int vs_arr[] = {5, 10, 16, 19 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, 119.719491);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800 };
    int vs_arr[] = {10, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -5.174450);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800 };
    int vs_arr[] = {10, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 7.221656);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {17, 18 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 20.800000, -0.202424, 0.202424, -5.710000 };
    int vs_arr[] = {3, 5, 10, 16, 18 };
    add_constraint(&ctx, 5, as_arr, vs_arr, 1, -28.472394);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {16, 17 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {17 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 10.632000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 17 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -73.590345);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 17 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 78.358905);
    }

    BLOCK {
    double as_arr[] = {20.800000, -0.202424, 0.202424, -20.800000 };
    int vs_arr[] = {5, 10, 16, 17 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, -99.792650);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 0.202424, -0.202424, 20.800000 };
    int vs_arr[] = {5, 10, 16, 17 };
    add_constraint(&ctx, 4, as_arr, vs_arr, 1, 109.271310);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800 };
    int vs_arr[] = {10, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -5.253117);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800 };
    int vs_arr[] = {10, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 7.300323);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {15, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800 };
    int vs_arr[] = {10, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -3.773941);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800 };
    int vs_arr[] = {10, 16 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 7.773941);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {14, 15 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {15 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 10.132000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 15 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -66.052503);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 15 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 70.821063);
    }

    BLOCK {
    double as_arr[] = {20.800000, -0.202424, -20.597576 };
    int vs_arr[] = {5, 10, 15 };
    add_constraint(&ctx, 3, as_arr, vs_arr, 1, -89.346788);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 0.202424, 20.597576 };
    int vs_arr[] = {5, 10, 15 };
    add_constraint(&ctx, 3, as_arr, vs_arr, 1, 98.825448);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800 };
    int vs_arr[] = {10, 15 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -3.799738);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800 };
    int vs_arr[] = {10, 15 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 8.799738);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {13, 14 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {14 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 9.632000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 14 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -58.514661);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 14 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 63.283221);
    }

    BLOCK {
    double as_arr[] = {20.800000, -0.202424, -20.597576 };
    int vs_arr[] = {5, 10, 14 };
    add_constraint(&ctx, 3, as_arr, vs_arr, 1, -78.896516);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 0.202424, 20.597576 };
    int vs_arr[] = {5, 10, 14 };
    add_constraint(&ctx, 3, as_arr, vs_arr, 1, 88.375176);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800 };
    int vs_arr[] = {10, 14 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -3.765608);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800 };
    int vs_arr[] = {10, 14 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 8.765608);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {12, 13 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {13 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 9.132000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 13 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -50.976865);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 13 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 55.745425);
    }

    BLOCK {
    double as_arr[] = {20.800000, -0.202424, -20.597576 };
    int vs_arr[] = {5, 10, 13 };
    add_constraint(&ctx, 3, as_arr, vs_arr, 1, -68.449952);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 0.202424, 20.597576 };
    int vs_arr[] = {5, 10, 13 };
    add_constraint(&ctx, 3, as_arr, vs_arr, 1, 77.928612);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800 };
    int vs_arr[] = {10, 13 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -3.675508);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800 };
    int vs_arr[] = {10, 13 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 8.675508);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {11, 12 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {12 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 8.632000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 12 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -43.438993);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 12 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 48.207553);
    }

    BLOCK {
    double as_arr[] = {20.800000, -0.202424, -20.597576 };
    int vs_arr[] = {5, 10, 12 };
    add_constraint(&ctx, 3, as_arr, vs_arr, 1, -58.086137);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 0.202424, 20.597576 };
    int vs_arr[] = {5, 10, 12 };
    add_constraint(&ctx, 3, as_arr, vs_arr, 1, 67.564797);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800 };
    int vs_arr[] = {10, 12 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -2.654928);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800 };
    int vs_arr[] = {10, 12 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 7.654928);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {10, 11 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {11 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 8.132000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 11 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -35.901120);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 11 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 40.669680);
    }

    BLOCK {
    double as_arr[] = {20.800000, -0.202424, -20.597576 };
    int vs_arr[] = {5, 10, 11 };
    add_constraint(&ctx, 3, as_arr, vs_arr, 1, -48.063607);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 0.202424, 20.597576 };
    int vs_arr[] = {5, 10, 11 };
    add_constraint(&ctx, 3, as_arr, vs_arr, 1, 57.542267);
    }

    BLOCK {
    double as_arr[] = {2.894800, -2.894800 };
    int vs_arr[] = {10, 11 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 0.291103);
    }

    BLOCK {
    double as_arr[] = {-2.894800, 2.894800 };
    int vs_arr[] = {10, 11 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 4.708897);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {9, 10 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {8, 9 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {9 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 7.618000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 9 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -28.152249);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 9 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 32.920809);
    }

    BLOCK {
    double as_arr[] = {20.800000, -20.800000 };
    int vs_arr[] = {5, 9 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -37.584790);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 20.800000 };
    int vs_arr[] = {5, 9 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 47.063450);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {7, 8 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {8 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 7.112000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 8 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -20.521165);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 8 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 25.289725);
    }

    BLOCK {
    double as_arr[] = {20.800000, -20.800000 };
    int vs_arr[] = {5, 8 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -26.999803);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 20.800000 };
    int vs_arr[] = {5, 8 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 36.478463);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {6, 7 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {7 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 6.606000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 7 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -12.885171);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 7 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 17.653731);
    }

    BLOCK {
    double as_arr[] = {20.800000, -20.800000 };
    int vs_arr[] = {5, 7 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -16.421733);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 20.800000 };
    int vs_arr[] = {5, 7 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 25.900393);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {5, 6 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {6 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 6.100000);
    }

    BLOCK {
    double as_arr[] = {15.090000, -15.090000 };
    int vs_arr[] = {3, 6 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -5.249147);
    }

    BLOCK {
    double as_arr[] = {-15.090000, 15.090000 };
    int vs_arr[] = {3, 6 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 10.017707);
    }

    BLOCK {
    double as_arr[] = {20.800000, -20.800000 };
    int vs_arr[] = {5, 6 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, -5.842880);
    }

    BLOCK {
    double as_arr[] = {-20.800000, 20.800000 };
    int vs_arr[] = {5, 6 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 1, 15.321540);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {4, 5 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {3, 4 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {2, 3 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {1, 2 };
    add_constraint(&ctx, 2, as_arr, vs_arr, -1, 0.000000);
    }

    BLOCK {
    double as_arr[] = {-1.000000, 1.000000 };
    int vs_arr[] = {0, 1 };
    add_constraint(&ctx, 2, as_arr, vs_arr, 0, 0.000000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {0 };
    add_constraint(&ctx, 1, as_arr, vs_arr, -1, 5.594000);
    }

    BLOCK {
    double as_arr[] = {1.000000 };
    int vs_arr[] = {0 };
    add_constraint(&ctx, 1, as_arr, vs_arr, 0, 5.594000);
    }

    double obj_val;
    double* var_values_arr = (double*) malloc(ctx.varcnt() * sizeof(double));
    if (solve(&ctx, &obj_val, var_values_arr)) {
        printf("TID(%d) --> %lf\n", (int) pthread_self(), obj_val);
    } else {
        printf("TID(%d) --> unsat\n", (int) pthread_self());
    }
}

#define NTHREADS 50

static void* loop(void*)
{
    int i = 0;
    do {
        solve();
    } while (++i < 100);
    return NULL;
}

int main(int argc, char* argv[])
{
    pthread_t threads[NTHREADS];

    for (int i = 0; i < NTHREADS; ++i) {
        pthread_create(&threads[i], NULL, &loop, NULL);
    }
    for (int i = 0; i < NTHREADS; ++i) {
        pthread_join(threads[i], NULL);
    }
    printf("all done\n");
    return 0;
}


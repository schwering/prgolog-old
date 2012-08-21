/* vim: ft=c ts=4 sw=4 et wm=0 tw=0
 * Copyright 2012 Christoph Schwering (schwering@kbsg.rwth-aachen.de)
 *
 * File: replay.c
 * Main author: schwering.
 *
 * Reads observations from stdin and emits them at time points according to the
 * observation timestamps.
 */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#if 0
static void klatschtgleich(FILE *fp)
{
    char a0[32], a1[32];
    double t0, t, x0, y0, x1, y1;

    t0 = -1.0;
    while (fscanf(fp, "%lf %s %lf %lf %s %lf %lf\n",
                &t, a0, &x0, &y0, a1, &x1, &y1) == 7) {
        if (t0 > -1) {
            usleep((useconds_t) (1e6 * (t - t0)));
        }
        fprintf(stdout, "%f %s %f %f %s %f %f\n",
                t, a0, x0, y0, a1, x1, y1);
        fflush(stdout);
        t0 = t;
    }
}
#endif

static void klatschtgleich2(FILE *fp)
{
    char kind, a0[32], a1[32];
    double t0, t, veloc0, yaw0, x0, y0, veloc1, yaw1, x1, y1;

    t0 = -1.0;
    while (fscanf(fp, "%c %lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\n",
                &kind, &t,
                a0, &veloc0, &yaw0, &x0, &y0,
                a1, &veloc1, &yaw1, &x1, &y1) == 12) {
        if (t0 > -1) {
            usleep((useconds_t) (1e6 * (t - t0)));
        }
        fprintf(stdout, "%c %lf %s %lf %lf %lf %lf %s %lf %lf %lf %lf\n",
                kind, t,
                a0, veloc0, yaw0, x0, y0,
                a1, veloc1, yaw1, x1, y1);
        fflush(stdout);
        t0 = t;
    }
}

int main(int argc, char *argv[])
{
    if (argc <= 1) {
        klatschtgleich2(stdin);
    } else {
        int i;

        for (i = 1; i < argc; ++i) {
            FILE *fp = fopen(argv[i], "r");
            klatschtgleich2(fp);
            fclose(fp);
        }
    }
    return 0;
}


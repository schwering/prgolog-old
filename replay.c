#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

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

int main(int argc, char *argv[])
{
	if (argc <= 1) {
		klatschtgleich(stdin);
	} else {
		int i;

		for (i = 1; i < argc; ++i) {
			FILE *fp = fopen(argv[i], "r");
			klatschtgleich(fp);
			fclose(fp);
		}
	}
	return 0;
}


#include <stdio.h>
#include <stdlib.h>



static
void mat_gen(int n, double (*a)[n][n])
{
	double tmp = 1.0 / n / n;

	for (int i = 0; i < n; ++i)
		for (int j = 0; j < n; ++j)
			(*a)[i][j] = tmp * (i - j) * (i + j);
}

static
void mat_mul(int n, int p, int m,
		double (* restrict c)[n][m],
		double (* restrict a)[n][p],
		double (* restrict b)[p][m])
{
	for (int i = 0; i < n; ++i)
		for (int k = 0; k < p; ++k)
			for (int j = 0; j < m; ++j)
				(*c)[i][j] += (*a)[i][k] * (*b)[k][j];
}

int main(int argc, char *argv[])
{
	int n = 1500;

	if (argc > 1)
		n = atoi(argv[1]);

	double (*a)[n][n] = malloc(sizeof *a);
	double (*b)[n][n] = malloc(sizeof *b);
	double (*c)[n][n] = calloc(1, sizeof *c);

	if (!a || !b || !c)
		return -1;

	mat_gen(n, a);
	mat_gen(n, b);
	mat_mul(n, n, n, c, a, b);

	printf("%f\n", (*c)[n >> 1][n >> 1]);

	free(a);
	free(b);
	free(c);

	return 0;
}


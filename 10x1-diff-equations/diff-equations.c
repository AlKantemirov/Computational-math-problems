#include <stdio.h>
#include <math.h>

double f(double x, double y);
void euler(double x0, double y0, double h, double x_end);

int main(int argc, char** argv) {
	double x0 = 1.0;
	double y0 = 2.0;
	double x_end = 3.0;
	double steps[] = {0.1, 0.01, 0.001, 0.0001};
	int steps_count = sizeof(steps) / sizeof(steps[0]);

	for (int i = 0; i< steps_count; i++) {
		printf("Step size: %.4f\n", steps[i]);
		euler(x0, y0, steps[i], x_end);
		printf("\n");
	}

	(void)argc;
	(void)argv;
	return 0;
}

double f(double x, double y) {
	return y / x - 4 / pow(x, 2);
}
void euler(double x0, double y0, double h, double x_end) {
	double x = x0;
	double y = y0;

	printf("x\t\ty(x)\n");
	while (x <= x_end) {
		printf("%.8f\t%.8f\n", x, y);
		y += h * f(x, y);
		x += h;
	}
}

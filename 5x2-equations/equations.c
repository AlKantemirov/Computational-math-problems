#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <stdbool.h>

#define PI 3.14159265358979323846

typedef struct Result {
	double value;
	int iterations;
} Result;

double f(double x);
double f_derivative(double x);
Result bisection(double (*func)(double), double a, double b, double epsilon);
Result simple_iteration(double x0, bool first, double epsilon);
Result newton(double (*func)(double), double (*func_der)(double), double x0, double epsilon);

int main(int argc, char** argv) {
	double epsilon;

	if (argc < 2) epsilon = 0.01;
	else epsilon = atof(argv[1]);

	Result bi_1 = bisection(f, 0.0, 0.5 * PI, epsilon);
	Result bi_2 = bisection(f, 0.5 * PI, 3 * PI, epsilon);
	Result si_1 = simple_iteration(0.5 * PI, true, epsilon);
	Result si_2 = simple_iteration(0.5 * PI, false, epsilon);
	Result nw_1 = newton(f, f_derivative, 0, epsilon);
	Result nw_2 = newton(f, f_derivative, 3 * PI, epsilon);
	printf("Bisection method\t\tFirst root: %.8f\t\tIterations: %d\n\t\t\t\tSecond root: %.8f\t\tIterations: %d\n", bi_1.value, bi_1.iterations, bi_2.value, bi_2.iterations);
	printf("Simple iteration method\t\tFirst root: %.8f\t\tIterations: %d\n\t\t\t\tSecond root: %.8f\t\tIterations: %d\n", si_1.value, si_1.iterations, si_2.value, si_2.iterations);
	printf("Newton method\t\t\tFirst root: %.8f\t\tIterations: %d\n\t\t\t\tSecond root: %.8f\t\tIterations: %d\n", nw_1.value, nw_1.iterations, nw_2.value, nw_2.iterations);

	return 0;
}

double f(double x) {
	return sin(x) - pow(x - PI / 2, 2);
}
double f_derivative(double x) {
	return cos(x) - 2 * (x - PI / 2);
}
Result bisection(double (*func)(double), double a, double b, double epsilon) {
	Result res = {};
	int iters = 0;
	double v;
	while ((b - a) / 2 > epsilon) {
		v = (a + b) / 2;
		if (func(v) == 0.0) break;
		iters++;
		if (func(v) * func(a) < 0) b = v;
		else a = v;
	}
	v = (a + b) / 2;
	res.value = v;
	res.iterations = iters;
	return res;
}
Result simple_iteration(double x0, bool first, double epsilon) {
	Result res = {};
	int iters = 0;
	double v;
	do {
		if (first) v = PI / 2 - pow(fabs(sin(x0)), 0.5);
		else v = PI / 2 + pow(fabs(sin(x0)), 0.5);
		iters++;
		if (fabs(v - x0) < epsilon) break;
		x0 = v;
	} while (true);
	res.value = v;
	res.iterations = iters;
	return res;
}
Result newton(double (*func)(double), double (*func_der)(double), double x0, double epsilon) {
	Result res = {};
	int iters = 0;
	double v;
	do {
		v = x0 - func(x0) / func_der(x0);
		iters++;
		if (fabs(v - x0) < epsilon) break;
		x0 = v;
	} while (true);
	res.value = v;
	res.iterations = iters;
	return res;
}

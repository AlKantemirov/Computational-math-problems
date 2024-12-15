#include <stdio.h>
#include <math.h>

#define MAX_PARTITIONS 500000
#define EPSILON 1E-6
#define TARGET_EPSILON 1E-5
#define TRUE_EXP (exp(1) - 1)
#define TRUE_COS sin(1)
#define TRUE_EXP_NEG_SQUARE 0.139383

double f_exp(double x);
double f_cos(double x);
double f_exp_neg_square(double x);
double left_rectangle(double (*f)(double), double a, double b, int n);
double middle_rectangle(double (*f)(double), double a, double b, int n);
double trapezoidal(double (*f)(double), double a, double b, int n);
double simpson(double (*f)(double), double a, double b, int n);
double relative_error(double true_value, double computed_value);

int main(int argc, char** argv) {
	double a = 0.0;
	double b = 1.0;

	for (int n = 1000; n <= MAX_PARTITIONS; n += 1000) {
		double left_res = left_rectangle(f_exp, a, b, n);
		double left_res_error = relative_error(TRUE_EXP, left_res);

		double mid_res = middle_rectangle(f_exp, a, b, n);
		double mid_res_error = relative_error(TRUE_EXP, mid_res);

		double trap_res = trapezoidal(f_cos, a, b, n);
		double trap_res_error = relative_error(TRUE_COS, trap_res);

		double simp_res = simpson(f_cos, a, b, n);
		double simp_res_error = relative_error(TRUE_COS, simp_res);

		if (left_res_error < EPSILON &&
			mid_res_error < EPSILON &&
			trap_res_error < EPSILON &&
			simp_res_error < EPSILON) {
			printf(
"Partitions: %d\n\
Left rectangle (exp): %lf\t\tError: %lf\n\
Middle rectangle (exp): %lf\tError: %lf\n\
Trapezoidal (cos): %lf\t\tError: %lf\n\
Simpson (cos): %lf\t\t\tError: %lf\n\n", n, 
				left_res, left_res_error,
				mid_res, mid_res_error,
				trap_res, trap_res_error,
				simp_res, simp_res_error);
			break;
		}
	}

	printf("--------------------\n\n");

	a = 1.0;
	b = 3.0;
	double approx_res;
	int partitions = 100;

	do {
		approx_res = simpson(f_exp_neg_square, a, b, partitions);
		partitions *= 2;
	} while (relative_error(TRUE_EXP_NEG_SQUARE, approx_res) >= TARGET_EPSILON);
	
	printf("Integral result for exp(-x^2) on [%.0f, %.0f]: %lf with %d partitions (Simpson)\n", a, b, approx_res, partitions / 2);

	(void)argc;
	(void)argv;
	return 0;
}

double f_exp(double x) {
	return exp(x);
}
double f_cos(double x) {
	return cos(x);
}
double f_exp_neg_square(double x) {
	return exp(-x * x);
}
double left_rectangle(double (*f)(double), double a, double b, int n) {
	double h = (b - a) / n;
	double sum = 0.0;
	for (int i = 0; i < n; i++) 
		sum += f(a + i * h);
	return sum * h;
}
double middle_rectangle(double (*f)(double), double a, double b, int n) {
	double h = (b - a) / n;
	double sum = 0.0;
	for (int i = 0; i < n; i++)
		sum += f(a + i * h + h / 2);
	return sum * h;
}
double trapezoidal(double (*f)(double), double a, double b, int n) {
	double h = (b - a) / n;
	double sum = (f(a) + f(b)) * 0.5;
	for (int i = 1; i < n; i++)
		sum += f(a + i * h);
	return sum * h;
}
double simpson(double (*f)(double), double a, double b, int n) {
	if (n % 2 != 0) n++;
	double h = (b - a) / n;
	double sum = f(a) + f(b);
	for (int i = 1; i < n; i++) {
		if (i % 2 == 0) sum += 2 * f(a + i * h);
		else sum += 4 * f(a + i * h);
	}
	return sum * h / 3.0;
}
double relative_error(double true_value, double computed_value) {
	return fabs((true_value - computed_value) / true_value);
}

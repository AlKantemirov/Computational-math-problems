#include <stdio.h>
#include <math.h>

#define EXP_DERIVATIVE M_E
#define COS_DERIVATIVE sin(-1)

double derivative_exp(double x0, double delta);
double derivative_cos(double x0, double delta);
double derivative_exp_central(double x0, double delta);
double derivative_cos_central(double x0, double delta);
double relative_error(double true_value, double computed_value);

int main(int argc, char** argv) {
	double x0 = 1.0;
	double deltas[] = {0.1, 0.01, 0.001, 0.0001, 0.00001};
	int deltas_num = sizeof(deltas) / sizeof(deltas[0]);

	printf("Delta\t\tExp der (f)\tError (f)\t\
Exp der (d)\tError (d)\t\
Exp der ctr (f)\tError (f)\t\
Exp der ctr (d)\tError (d)\n");

	for (int i = 0; i < deltas_num; i++) {
		float delta_f = (float)deltas[i];
		double delta_d = deltas[i];
		
		float exp_deriv_f = derivative_exp(x0, delta_f);
		double exp_deriv_d = derivative_exp(x0, delta_d);
		float exp_deriv_central_f = derivative_exp_central(x0, delta_f);
		double exp_deriv_central_d = derivative_exp_central(x0, delta_d);

		float error_f = relative_error(EXP_DERIVATIVE, exp_deriv_f);
		double error_d = relative_error(EXP_DERIVATIVE, exp_deriv_d);
		float error_central_f = relative_error(EXP_DERIVATIVE, exp_deriv_central_f);
		double error_central_d = relative_error(EXP_DERIVATIVE, exp_deriv_central_d);

		printf("%e\t%f\t%f\t%lf\t%lf\t%f\t%f\t%lf\t%lf\n", delta_d, 
				exp_deriv_f, error_f, 
				exp_deriv_d, error_d, 
				exp_deriv_central_f, error_central_f,
				exp_deriv_central_d, error_central_d);
	}

	printf("\n\n");

	printf("Delta\t\tCos der (f)\tError (f)\t\
Cos der (d)\tError (d)\t\
Cos der ctr (f)\tError (f)\t\
Cos der ctr (d)\tError (d)\n");

	for (int i = 0; i < deltas_num; i++) {
		float delta_f = (float)deltas[i];
		double delta_d = deltas[i];
		
		float cos_deriv_f = derivative_cos(x0, delta_f);
		double cos_deriv_d = derivative_cos(x0, delta_d);
		float cos_deriv_central_f = derivative_cos_central(x0, delta_f);
		double cos_deriv_central_d = derivative_cos_central(x0, delta_d);

		float error_f = relative_error(COS_DERIVATIVE, cos_deriv_f);
		double error_d = relative_error(COS_DERIVATIVE, cos_deriv_d);
		float error_central_f = relative_error(COS_DERIVATIVE, cos_deriv_central_f);
		double error_central_d = relative_error(COS_DERIVATIVE, cos_deriv_central_d);

		printf("%e\t%f\t%f\t%lf\t%lf\t%f\t%f\t%lf\t%lf\n", delta_d, 
				cos_deriv_f, error_f, 
				cos_deriv_d, error_d, 
				cos_deriv_central_f, error_central_f,
				cos_deriv_central_d, error_central_d);
	}
	
	(void) argv;
	(void) argc;
	return 0;
}

double derivative_exp(double x0, double delta) {
	return (exp(x0 + delta) - exp(x0)) / delta;
}
double derivative_cos(double x0, double delta) {
	return (cos(x0 + delta) - cos(x0)) / delta;
}
double derivative_exp_central(double x0, double delta) {
	return (exp(x0 + delta) - exp(x0 - delta)) / (2 * delta);
}
double derivative_cos_central(double x0, double delta) {
	return (cos(x0 + delta) - cos(x0 - delta)) / (2 * delta);
}
double relative_error(double true_value, double computed_value) {
	return fabs((true_value - computed_value) / true_value);
}

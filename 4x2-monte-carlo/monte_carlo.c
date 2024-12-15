#include <raylib.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define SCREEN_WIDTH 800
#define SCREEN_HEIGHT 600

double f1(double x);
double f2(double x);

int main(int argc, char** argv) {
	int point_iterations;
	char computed_value_text[32];
	double computed_value;
	double x_min = 0.0;
	double x_max = PI;
	Rectangle computed_value_pos = {(int)(SCREEN_WIDTH / 2) - 45, (int)(SCREEN_HEIGHT * 3 / 4), 30, 30};

	if (argc < 2) point_iterations = 100;
	else point_iterations = atoi(argv[1]);

	InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, "Площади фигуры (метод Монте-Карло)");

	SetTargetFPS(60);

	while (!WindowShouldClose()) {
		int inside_points = 0;

		BeginDrawing();

		ClearBackground(RAYWHITE);
		for (int x = 0; x < SCREEN_WIDTH; x++) {
			double real_x = (double)x / SCREEN_WIDTH * (x_max - x_min) + x_min;
			double graph_1_y = (double)SCREEN_HEIGHT / 2 - (int)(f1(real_x) * 100);
			double graph_2_y = (double)SCREEN_HEIGHT / 2 - (int)(f2(real_x) * 100);
			DrawPixel(x, graph_1_y, BLUE);
			DrawPixel(x, graph_2_y, RED);

			for (int i = 0; i < point_iterations / SCREEN_WIDTH; i++) {
				Vector2 point = {GetRandomValue(0, SCREEN_WIDTH), GetRandomValue(0, SCREEN_HEIGHT / 2)};
				DrawCircleV(point, 1, BLACK);
				if (point.y < graph_1_y && point.y > graph_2_y)
					inside_points++;
			}
		}

		computed_value = (inside_points / (double) point_iterations) * ((x_max - x_min) * 1.5);
		sprintf(computed_value_text, "%.8f", computed_value);

		DrawText(computed_value_text, computed_value_pos.x, computed_value_pos.y, 26, BLACK);

		EndDrawing();
		WaitTime(0.5);
	}

	CloseWindow();

	(void)argc;
	(void)argv;
	return 0;
}

double f1(double x) {
	return sin(x);
}
double f2(double x) {
	return (x - PI / 2) * (x - PI / 2);
}

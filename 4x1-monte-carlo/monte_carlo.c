#include <raylib.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define SCREEN_WIDTH 800
#define SCREEN_HEIGHT 600
#define SQUARE_SIDE 500

double relative_error(double true_v, double comp_v);

int main(int argc, char** argv) {
	int point_iterations;
	int circle_collisions;
	char computed_value_text[16];
	char error_text[16];
	double computed_value;
	double error;

	if (argc < 2) point_iterations = 100;
	else point_iterations = atoi(argv[1]);

	InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, "Число пи (метод Монте-Карло)");

	Rectangle square = {0, (int)(SCREEN_HEIGHT / 2) - (int)(SQUARE_SIDE / 2), SQUARE_SIDE, SQUARE_SIDE};
	Vector2 circle_pos = {(int)(SQUARE_SIDE / 2), (int)(SCREEN_HEIGHT / 2)};
	Vector2 computed_value_pos = {(int)(SCREEN_WIDTH * 3 / 4), (int)(SCREEN_HEIGHT / 2) - 100};
	Vector2 error_pos = {computed_value_pos.x, computed_value_pos.y + 200};

	SetTargetFPS(60);

	while (!WindowShouldClose()) {

		circle_collisions = 0;
		Vector2 coll_point = {};

		BeginDrawing();

		ClearBackground(RAYWHITE);
		DrawRectangleLinesEx(square, 3.0f, BLACK);
		DrawCircleLinesV(circle_pos, (int)(SQUARE_SIDE / 2), RED);

		for (int i = 0; i < point_iterations; i++) {
			coll_point.x = GetRandomValue(1, SQUARE_SIDE);
			coll_point.y = GetRandomValue(SCREEN_HEIGHT / 2 - SQUARE_SIDE / 2 + 1, SCREEN_HEIGHT / 2 + SQUARE_SIDE / 2);

			DrawCircleV(coll_point, 1, BLACK);

			if (CheckCollisionPointCircle(coll_point, circle_pos, (int)(SQUARE_SIDE / 2)))
				circle_collisions++;
		}

		computed_value = 4.0 * circle_collisions / point_iterations;
		sprintf(computed_value_text, "%.8f", computed_value);
		error = relative_error(PI, computed_value);
		sprintf(error_text, "Error:\n%.8f", error);

		DrawText(computed_value_text, computed_value_pos.x, computed_value_pos.y, 24, BLACK);
		DrawText(error_text, error_pos.x, error_pos.y, 24, BLACK);

		EndDrawing();
		WaitTime(0.5);
	}

	CloseWindow();

	(void)argc;
	(void)argv;
	return 0;
}

double relative_error(double true_v, double comp_v) {
	return fabs((true_v - comp_v) / true_v);
}

#include <math.h>
#include <raylib.h>

#define MAX_SIZE 32

typedef struct {
    float matrix[MAX_SIZE][MAX_SIZE];
    float results[MAX_SIZE];
    int size;
} LinearSystem;

void InitLinearSystem(LinearSystem *system, int size) {
    system->size = size;
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            system->matrix[i][j] = 0.0f;
        }
        system->results[i] = 0.0f;
    }
}

void GaussianElimination(LinearSystem *system) {
    for (int i = 0; i < system->size; i++) {
        for (int j = i + 1; j < system->size; j++) {
            float ratio = system->matrix[j][i] / system->matrix[i][i];
            for (int k = 0; k < system->size; k++) {
                system->matrix[j][k] -= ratio * system->matrix[i][k];
            }
            system->results[j] -= ratio * system->results[i];
        }
    }
}

void GaussianEliminationWithPivoting(LinearSystem *system) {
    for (int i = 0; i < system->size; i++) {
        int maxRow = i;
        for (int k = i + 1; k < system->size; k++) {
            if (fabs(system->matrix[k][i]) > fabs(system->matrix[maxRow][i])) {
                maxRow = k;
            }
        }
        for (int k = 0; k < system->size; k++) {
            float temp = system->matrix[maxRow][k];
            system->matrix[maxRow][k] = system->matrix[i][k];
            system->matrix[i][k] = temp;
        }
        float temp = system->results[maxRow];
        system->results[maxRow] = system->results[i];
        system->results[i] = temp;

        for (int j = i + 1; j < system->size; j++) {
            float ratio = system->matrix[j][i] / system->matrix[i][i];
            for (int k = 0; k < system->size; k++) {
                system->matrix[j][k] -= ratio * system->matrix[i][k];
            }
            system->results[j] -= ratio * system->results[i];
        }
    }
}

void BackSubstitution(LinearSystem *system, float *solution) {
    for (int i = system->size - 1; i >= 0; i--) {
        solution[i] = system->results[i];
        for (int j = i + 1; j < system->size; j++) {
            solution[i] -= system->matrix[i][j] * solution[j];
        }
        solution[i] /= system->matrix[i][i];
    }
}

void DrawMatrix(LinearSystem *system, int x, int y) {
    for (int i = 0; i < system->size; i++) {
        for (int j = 0; j < system->size; j++) {
            DrawText(TextFormat("%.2f", system->matrix[i][j]), x + j * 50, y + i * 20, 20, BLACK);
        }
        DrawText(TextFormat("= %.2f", system->results[i]), x + system->size * 50, y + i * 20, 20, BLACK);
    }
}

void DrawSolution(float *solution, int size, int x, int y) {
    for (int i = 0; i < size; i++) {
        DrawText(TextFormat("x[%d] = %.2f", i, solution[i]), x, y + i * 20, 20, BLACK);
    }
}

int main(void) {
    const int screenWidth = 800;
    const int screenHeight = 600;

    InitWindow(screenWidth, screenHeight, "Gaussian Elimination Solver");

    LinearSystem system = { { 
		{1.0, 2.0, 3.0}, 
		{2.0, 3.0, 1.0}, 
		{3.0, 1.0, 2.0} },
				 {9.0, 8.0, 7.0},
				 3 };

    //InitLinearSystem(&system, system.size);

    float solution[MAX_SIZE];
    bool showSolution = false;

    while (!WindowShouldClose()) {
        if (IsKeyPressed(KEY_ENTER)) {
            GaussianElimination(&system);
        }

        if (IsKeyPressed(KEY_P)) {
			GaussianEliminationWithPivoting(&system);
        }

        if (IsKeyPressed(KEY_B)) { 
			BackSubstitution(&system, solution);
            showSolution = true;
        }

        if (IsKeyPressed(KEY_F)) { 
			GaussianElimination(&system);
            BackSubstitution(&system, solution);
            showSolution = true;
        }

        BeginDrawing();
        ClearBackground(RAYWHITE);

        DrawText("Gaussian Elimination Solver", 10, 10, 20, DARKGRAY);
        
        DrawMatrix(&system, 10, 50);

        if (showSolution) {
            DrawSolution(solution, system.size, 10, 400);
        }

        DrawText("Press ENTER to perform Gaussian elimination", 10, 300, 20, DARKGRAY);
        DrawText("Press P to perform Gaussian elimination with pivoting", 10, 320, 20, DARKGRAY);
        DrawText("Press B to perform back substitution", 10, 340, 20, DARKGRAY);
        DrawText("Press F to perform full Gaussian elimination and back substitution", 10, 360, 20, DARKGRAY);

        EndDrawing();
    }

    CloseWindow();
    return 0;
}

#
#!/bin/sh
#
gcc monte_carlo.c -o monte_carlo -Wall -Wextra -lraylib -lpthread -ldl -lrt -lX11 -lm
./monte_carlo $1

#
#!/bin/sh
#
gcc equations.c -o equations -Wall -Wextra -lm
./equations $1

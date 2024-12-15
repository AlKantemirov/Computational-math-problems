#
#!/bin/sh
#
gcc gauss.c -o gauss -Wall -Wextra -lraylib -lpthread -ldl -lrt -lX11 -lm
./gauss

#include <unistd.h>

int main(int argc, char *argv[]) {
    char * args[2] = { "ok", NULL };
    execv("./ok", args);
}

#include <unistd.h>

int main(int argc, char *argv[]) {
    char *args[1] = {NULL};
    execv("./T7037", args);
}

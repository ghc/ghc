#include <stddef.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
#ifdef __MINGW32__
    const
#endif
    char * args[2] = {"T7037", NULL};
    execv("./T7037", args);
}


#include <stdio.h>

void debugLn(char *s) {
    printf("%s\n", s);
}

void debugErrLn(char *s) {
    fprintf(stderr, "%s\n", s);
}

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <shell-tools.c>

int main(int argc, char **argv) {
    char **args;
    args = malloc(sizeof(char *) * (argc + 3));
    if (args == NULL) {
        fprintf(stderr, "Malloc failed\n");
        exit(1);
    }
    args[0] = "GHC_PATH"; /* Gets replaced by sed */
    args[1] = "-BTOP_ABS"; /* Gets replaced by sed */
    args[2] = "-fhardwire-lib-paths";
    if ((argc >= 2) && (strcmp(argv[1], "-v") == 0)) {
        printf("Using %s %s %s\n", args[0], args[1], args[2]);
        fflush(stdout);
    }
    memcpy(args + 3, argv + 1, sizeof(char *) * (argc - 1));
    args[argc+2] = NULL;
    return run(argv[0],
               "GHC_PATH", /* Gets replaced by sed */
               argc + 2,
               args);
}

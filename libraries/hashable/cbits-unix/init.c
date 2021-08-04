#include <stdint.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

uint64_t hs_hashable_init() {

    /* if there is /dev/urandom, read from it */
    FILE *urandom = fopen("/dev/urandom", "r");
    if (urandom) {
        uint64_t result = 0;
        size_t r = fread(&result, sizeof(uint64_t), 1, urandom);
        fclose(urandom);

        if (r == 1) {
            return result;
        } else {
            return 0xfeed1000;
        }

    } else {
        /* time of day */
        struct timeval tp = {0, 0};
        gettimeofday(&tp, NULL);

        /* cputime */
        clock_t c = clock();

        /* process id */
        pid_t p = getpid();

        return ((uint64_t) tp.tv_sec)
            ^ ((uint64_t) tp.tv_usec)
            ^ ((uint64_t) c << 16)
            ^ ((uint64_t) p << 32);
    }
}

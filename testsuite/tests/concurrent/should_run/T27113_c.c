#include <errno.h>
#include <time.h>
#include <unistd.h>

static long now_ns(void) {
    struct timespec t;
    while (clock_gettime(CLOCK_MONOTONIC, &t) != 0) {
        if (errno != EINTR) return 0;
    }
    return (long)t.tv_sec * 1000000000L + (long)t.tv_nsec;
}

int cpu_then_pause(long spin_ns) {
    long deadline = now_ns() + spin_ns;
    while (now_ns() < deadline) {
    }
    return pause();
}

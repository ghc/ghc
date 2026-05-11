#include <errno.h>
#include <stdint.h>
#include <time.h>
#include <unistd.h>

static int64_t now_ns(void) {
    struct timespec t;
    while (clock_gettime(CLOCK_MONOTONIC, &t) != 0) {
        if (errno != EINTR) return 0;
    }
    return (int64_t)t.tv_sec * 1000000000LL + (int64_t)t.tv_nsec;
}

int cpu_then_pause(int64_t spin_ns) {
    int64_t deadline = now_ns() + spin_ns;
    while (now_ns() < deadline) {
    }
    return pause();
}

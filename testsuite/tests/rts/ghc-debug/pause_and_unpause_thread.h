#include <time.h>

struct PauseTimestamps{
    time_t begin;
    time_t end;
};

void pauseAndUnpause(void);
time_t getPauseBegin();
time_t getPauseEnd();
time_t getUnixTime();

#include <time.h>

struct PauseTimestamps{
    time_t begin;
    time_t end;
};

void pauseAndUnpause(void);
time_t getPauseBegin(void);
time_t getPauseEnd(void);
time_t getUnixTime(void);

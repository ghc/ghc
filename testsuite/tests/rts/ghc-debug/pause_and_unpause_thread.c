#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include "pause_and_unpause_thread.h"
#include "Rts.h"
#include "RtsAPI.h"

#include <stdio.h>

struct PauseTimestamps timestamps = {0, 0};

void pauseAndUnpause(void){
    RtsPaused r_paused = rts_pause();

    if(!rts_isPaused()) {
        errorBelch("Expected the RTS to be paused.");
        exit(1);
    }

    timestamps.begin = time(NULL);
    sleep(5);
    timestamps.end = time(NULL);

    rts_unpause(r_paused);

    if(rts_isPaused()) {
        errorBelch("Expected the RTS to be unpaused.");
        exit(1);
    }

    return NULL;
}

time_t getPauseBegin(void) {
    return timestamps.begin;
}

time_t getPauseEnd(void) {
    return timestamps.end;
}

time_t getUnixTime(void){
    return time(NULL);
}

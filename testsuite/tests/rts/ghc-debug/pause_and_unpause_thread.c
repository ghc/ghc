#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include "pause_and_unpause_thread.h"
#include "Rts.h"
#include "RtsAPI.h"

#include <stdio.h>

struct PauseTimestamps timestamps = {0, 0};

void* pauseAndUnpause_thread(void* unused){
    RtsPaused r_paused = rts_pause();

    timestamps.begin = time(NULL);
    sleep(5);
    timestamps.end = time(NULL);

    rts_unpause(r_paused);

    return NULL;
}

void pauseAndUnpause(void){
    pthread_t threadId;
    pthread_create(&threadId, NULL, &pauseAndUnpause_thread, NULL);
    pthread_detach(threadId);
}

time_t getPauseBegin() {
    return timestamps.begin;
}

time_t getPauseEnd() {
    return timestamps.end;
}

time_t getUnixTime(){
    return time(NULL);
}

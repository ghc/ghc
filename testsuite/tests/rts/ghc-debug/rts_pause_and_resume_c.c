#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include "rts_pause_and_resume_c.h"
#include "Rts.h"
#include "RtsAPI.h"

#include <stdio.h>

struct PauseTimestamps timestamps = {0, 0};

void* pauseAndResume_thread(void* unused){
    RtsPaused rtsPaused = rts_pause();

    if(!rts_isPaused()) {
        errorBelch("Expected the RTS to be paused.");
        exit(1);
    }

    timestamps.begin = time(NULL);
    sleep(5);
    timestamps.end = time(NULL);

    rts_resume(rtsPaused);

    if(rts_isPaused()) {
        errorBelch("Expected the RTS to be resumed.");
        exit(1);
    }

    return NULL;
}

void pauseAndResume(void){
    pauseAndResume_thread(NULL);
}

void pauseAndResumeViaNewThread(void){
    pthread_t threadId;
    pthread_create(&threadId, NULL, &pauseAndResume_thread, NULL);
    pthread_detach(threadId);
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

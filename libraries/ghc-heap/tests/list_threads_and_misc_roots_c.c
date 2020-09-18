#include <stdio.h>
#include <stdlib.h>
#include "Rts.h"
#include "RtsAPI.h"
#include "list_threads_and_misc_roots_c.h"

int tsoCount = 0;
StgTSO** tsos;

int miscRootsCount = 0;
StgClosure** miscRoots;

void collectTSOsCallback(void *user, StgTSO* tso){
    tsoCount++;
    tsos = realloc(tsos, sizeof(StgTSO*) * tsoCount);
    tsos[tsoCount - 1] = tso;
}

void collectMiscRootsCallback(void *user, StgClosure* closure){
    miscRootsCount++;
    miscRoots = realloc(miscRoots, sizeof(StgClosure*) * miscRootsCount);
    miscRoots[miscRootsCount - 1] = closure;
}

void* listThreads_thread(void* unused){
    RtsPaused paused = rts_pause();
    rts_listThreads(&collectTSOsCallback, NULL);
    rts_listMiscRoots(&collectMiscRootsCallback, NULL);
    rts_resume(paused);

    return NULL;
}

void listThreadsAndMiscRoots(void){
    pthread_t threadId;
    pthread_create(&threadId, NULL, &listThreads_thread, NULL);
    pthread_join(threadId, NULL);
}

int getTSOCount(void){
    return tsoCount;
}

StgTSO** getTSOs(void){
    return tsos;
}

int getMiscRootsCount(void){
    return miscRootsCount;
}

StgClosure** getMiscRoots(void){
    return miscRoots;
}

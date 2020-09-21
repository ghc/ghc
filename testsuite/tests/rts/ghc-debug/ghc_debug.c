#include <stdio.h>
#include <unistd.h>

#include "Rts.h"
#include "RtsAPI.h"

#include "ghc_debug.h"

void expectNoChange(const char * msg, volatile unsigned int * count);
void expectChange(const char * msg, volatile unsigned int * count);

// Test rts_pause/rts_resume by observing a count that we expect to be
// incremented by concurrent Haskell thread(s). We expect rts_pause to stop
// those threads and hence stop incrementing the count.
void pauseAndResume
    ( bool assertNotPaused // [in] True to enable assertions before rts_pause and after rts_resume.
                           // Often disabled when calling this concurrently.
    , volatile unsigned int * count  // [in] Haskell threads should be forever incrementing this.
    )
{
    // Assert the RTS is resumed.
    if (assertNotPaused)
    {
        expectChange("RTS should be running", count);
        if(rts_isPaused()) {
            errorBelch("Expected the RTS to be resumed.");
            exit(1);
        }
    }

    // Pause and assert.
    rts_pause();
    if(!rts_isPaused()) {
        errorBelch("Expected the RTS to be paused.");
        exit(1);
    }

    expectNoChange("RTS should be paused", count);

    // Resume.
    rts_resume();

    // Assert the RTS is resumed.
    if (assertNotPaused)
    {
        expectChange("RTS should be resumed", count);
        if(rts_isPaused()) {
            errorBelch("Expected the RTS to be resumed.");
            exit(1);
        }
    }
}

void* pauseAndResumeViaThread_helper(volatile unsigned int * count)
{
    pauseAndResume(false, count);
    return NULL;
}

// Call pauseAndResume via a new thread and return the thread ID.
unsigned long pauseAndResumeViaThread
    ( volatile unsigned int * count  // [in] Haskell threads should be forever incrementing this.
    )
{
    pthread_t threadId;
    pthread_create(&threadId, NULL, &pauseAndResumeViaThread_helper, count);
    return threadId;
}

const int TIMEOUT = 1000000; // 1 second

// Wait for &count to change (else exit(1) after TIMEOUT).
void expectChange(const char * msg, volatile unsigned int * count)
{
    unsigned int count_0 = *count;
    int microSecondsLeft = TIMEOUT;
    unsigned int sleepTime = 10000;
    while (true)
    {
        usleep(sleepTime);
        microSecondsLeft -= sleepTime;

        if (count_0 != *count)
        {
            // Change detected.
            return;
        }

        if (microSecondsLeft < 0)
        {
            printf("Expected: %s\n", msg);
            exit(1);
        }
    }
}

// Ensure &count does NOT change (for TIMEOUT else exit(1)).
void expectNoChange(const char * msg, volatile unsigned int * count)
{
    unsigned int count_0 = *count;
    int microSecondsLeft = TIMEOUT;
    unsigned int sleepTime = 10000;
    while (true)
    {
        usleep(sleepTime);
        microSecondsLeft -= sleepTime;

        if (count_0 != *count)
        {
            // Change detected.
            printf("Expected: %s\n", msg);
            exit(1);
        }

        if (microSecondsLeft < 0)
        {
            return;
        }
    }
}

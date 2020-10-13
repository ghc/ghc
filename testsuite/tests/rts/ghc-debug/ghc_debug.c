#include <assert.h>
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
    Capability * cap = rts_pause();
    if(cap == NULL) {
        errorBelch("rts_pause() returned NULL.");
        exit(1);
    }
    Capability * cap2 = rts_pause(); // This should have no effect and return immediately.
    if(cap != cap2) {
        errorBelch("A second call to rts_pause() returned a different Capability.");
        exit(1);
    }

    if(!rts_isPaused()) {
        errorBelch("Expected the RTS to be paused.");
        exit(1);
    }

    expectNoChange("RTS should be paused", count);

    // Resume.
    rts_resume(cap);

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

int addOne(int a)
{
    return a + 1;
}

// Pause tht RTS and call all RtsAPI.h functions.
void pauseAndUseRtsAPIAndResume
    ( HaskellObj haskellFn          // [in] A Haskell function (StablePtr (a -> a))
    , HaskellObj haskellFnArgument  // [in] An argument to apply to haskellFn (a)
    , HaskellObj obj1  // [in] arbitrary haskell value to evaluate of arbitrary type.
    , HaskellObj obj2  // [in] arbitrary haskell value to evaluate of arbitrary type.
    , HsStablePtr stablePtrIO  // [in] arbitrary haskell IO action to execute (StablePtr (IO t))
    )
{
    // Pause the RTS.
    printf("Pause the RTS...");
    Capability * cap = rts_pause();
    printf("Paused\n");

    // Note the original capability. We assert that cap is not changed by
    // functions that take &cap.
    Capability *const cap0 = cap;

    // Call RtsAPI.h functions

    // TODO print out what funciton is running to give better debug output if one of these deadlocks

    printf("getRTSStats...\n");
    RTSStats s;
    getRTSStats (&s);
    printf("getRTSStatsEnabled...\n");
    getRTSStatsEnabled();
    printf("getAllocations...\n");
    getAllocations();
    printf("rts_getSchedStatus...\n");
    rts_getSchedStatus(cap);
    printf("rts_getChar, rts_mkChar...\n");
    rts_getChar     (rts_mkChar       ( cap, 0 ));
    printf("rts_getInt, rts_mkInt...\n");
    rts_getInt      (rts_mkInt        ( cap, 0 ));
    printf("rts_getInt8, rts_mkInt8...\n");
    rts_getInt8     (rts_mkInt8       ( cap, 0 ));
    printf("rts_getInt16, rts_mkInt16...\n");
    rts_getInt16    (rts_mkInt16      ( cap, 0 ));
    printf("rts_getInt32, rts_mkInt32...\n");
    rts_getInt32    (rts_mkInt32      ( cap, 0 ));
    printf("rts_getInt64, rts_mkInt64...\n");
    rts_getInt64    (rts_mkInt64      ( cap, 0 ));
    printf("rts_getWord, rts_mkWord...\n");
    rts_getWord     (rts_mkWord       ( cap, 0 ));
    printf("rts_getWord8, rts_mkWord8...\n");
    rts_getWord8    (rts_mkWord8      ( cap, 0 ));
    printf("rts_getWord16, rts_mkWord16...\n");
    rts_getWord16   (rts_mkWord16     ( cap, 0 ));
    printf("rts_getWord32, rts_mkWord32...\n");
    rts_getWord32   (rts_mkWord32     ( cap, 0 ));
    printf("rts_getWord64, rts_mkWord64...\n");
    rts_getWord64   (rts_mkWord64     ( cap, 0 ));
    printf("rts_getPtr, rts_mkPtr...\n");
    int x = 0;
    rts_getPtr      (rts_mkPtr        ( cap, &x));
    printf("rts_getFunPtr, rts_mkFunPtr...\n");
    rts_getFunPtr   (rts_mkFunPtr     ( cap, &addOne ));
    printf("rts_getFloat, rts_mkFloat...\n");
    rts_getFloat    (rts_mkFloat      ( cap, 0.0 ));
    printf("rts_getDouble, rts_mkDouble...\n");
    rts_getDouble   (rts_mkDouble     ( cap, 0.0 ));
    printf("rts_getStablePtr, rts_mkStablePtr...\n");
    rts_getStablePtr (rts_mkStablePtr ( cap, &x ));
    printf("rts_getBool, rts_mkBool...\n");
    rts_getBool     (rts_mkBool       ( cap, 0 ));
    printf("rts_mkString...\n");
    rts_mkString     ( cap, "Hello ghc-debug!" );
    printf("rts_apply...\n");
    rts_apply        ( cap, deRefStablePtr(haskellFn), haskellFnArgument );

    printf("rts_eval...\n");
    HaskellObj ret;
    rts_eval(&cap, obj1, &ret);
    assert(cap == cap0);

    printf("rts_eval_...\n");
    rts_eval_ (&cap, obj2, 50, &ret);
    assert(cap == cap0);

    printf("rts_evalIO...\n");
    HaskellObj io = deRefStablePtr(stablePtrIO);
    rts_evalIO (&cap, io, &ret);
    assert(cap == cap0);

    printf("rts_evalStableIOMain...\n");
    HsStablePtr retStablePtr;
    rts_evalStableIOMain (&cap, stablePtrIO, &retStablePtr);
    assert(cap == cap0);

    printf("rts_evalStableIO...\n");
    rts_evalStableIO (&cap, stablePtrIO, &retStablePtr);
    assert(cap == cap0);

    printf("rts_evalLazyIO...\n");
    rts_evalLazyIO (&cap, io, &ret);
    assert(cap == cap0);

    printf("rts_evalLazyIO_...\n");
    rts_evalLazyIO_ (&cap,  io, 50, &ret);
    assert(cap == cap0);

    printf("rts_setInCallCapability...\n");
    rts_setInCallCapability (0, 1);
    printf("rts_pinThreadToNumaNode...\n");
    rts_pinThreadToNumaNode (0);

    // Resume the RTS.
    printf("Resume the RTS...");
    rts_resume(cap);
    assert(cap == cap0);
    printf("Resumed\n");
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

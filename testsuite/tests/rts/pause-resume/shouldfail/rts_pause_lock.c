#include <stdio.h>
#include <unistd.h>

#include "Rts.h"
#include "RtsAPI.h"

#include "rts_pause_lock.h"

// Although we expect errors rather than deadlock, we don't want a failed test
// to be a deadlocked test. Hence we use this as a 1 second timeout mechanism.
void assertDoneAfterOneSecond(int * done)
{
  sleep(1);
  if (!*done)
  {
    printf("Deadlock detected.");
    exit(1);
  }
}

void lockThenPause (int * done) {
  printf("Locking...");
  Capability * lockCap = rts_lock();
  printf("Locked\n");

  printf("Pausing...");
  PauseToken * token = rts_pause();
  Capability * pauseCap = pauseTokenCapability(token);
  printf("Paused\n");

  printf("Resuming...");
  rts_resume(token);
  printf("Resumed\n");

  printf("Unlocking...");
  rts_unlock(lockCap);
  printf("Unlocked\n");

  *done = 1;
}

void pauseThenLock (int * done) {
  printf("Pausing...");
  PauseToken * token = rts_pause();
  Capability * pauseCap = pauseTokenCapability(token);
  printf("Paused\n");

  printf("Locking...");
  Capability * lockCap = rts_lock();
  printf("Locked\n");

  printf("Unlocking...");
  rts_unlock(lockCap);
  printf("Unlocked\n");

  printf("Resuming...");
  rts_resume(token);
  printf("Resumed\n");

  *done = 1;
}

void doublePause (int * done) {
  printf("Pausing...");
  PauseToken * tokenA = rts_pause();
  Capability * pauseCapA = pauseTokenCapability(tokenA);
  printf("Paused\n");

  printf("Pausing...");
  PauseToken * tokenB = rts_pause();
  Capability * pauseCapB = pauseTokenCapability(tokenB);
  printf("Paused\n");

  printf("Resuming...");
  rts_resume(tokenA);
  printf("Resuming\n");

  printf("Resuming...");
  rts_resume(tokenB);
  printf("Resumed\n");

  *done = 1;
}

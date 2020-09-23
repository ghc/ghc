#include <stdio.h>
#include <unistd.h>

#include "Rts.h"
#include "RtsAPI.h"

#include "deadlock.h"

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
  Capability * pauseCap = rts_pause();
  printf("Paused\n");

  printf("Resuming...");
  rts_resume(pauseCap);
  printf("Resumed\n");

  printf("Unlocking...");
  rts_unlock(lockCap);
  printf("Unlocked\n");

  *done = 1;
}

void pauseThenLock (int * done) {
  printf("Pausing...");
  Capability * pauseCap = rts_pause();
  printf("Paused\n");

  printf("Locking...");
  Capability * lockCap = rts_lock();
  printf("Locked\n");

  printf("Unlocking...");
  rts_unlock(lockCap);
  printf("Unlocked\n");

  printf("Resuming...");
  rts_resume(pauseCap);
  printf("Resumed\n");

  *done = 1;
}

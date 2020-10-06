#include "HsFFI.h"

#include <stdio.h>
#include "Rts.h"
#include <pthread.h>

#define THREADS 6
#define OK 9999
static OSThreadId ids[THREADS];
static int results[THREADS];
static int waiters = 0;
static int done = 0;
static Condition cond;
static Mutex mutex;

HsInt capTest();

void* OSThreadProcAttr go(void *info)
{
  int cap;
  int res;
  int threadNum = *(int*)(info);

  // divide everything onto two caps (if there are two)
  cap = (threadNum % 2) % enabled_capabilities;

  OS_ACQUIRE_LOCK(&mutex);
  waiters++;
  if (waiters == THREADS) {
    broadcastCondition(&cond);
  } else {
    while(waiters != THREADS) {
      waitCondition(&cond, &mutex);
    }
  }
  OS_RELEASE_LOCK(&mutex);

  rts_setInCallCapability(cap, 0);
  res = capTest();
  *(int*)info = res == cap ? OK : res;
  OS_ACQUIRE_LOCK(&mutex);
  done++;
  broadcastCondition(&cond);
  OS_RELEASE_LOCK(&mutex);
  return 0;
}

int main(int argc, char *argv[])
{
  int n;
  bool ok;
  hs_init(&argc, &argv);
  initCondition(&cond);
  initMutex(&mutex);
  waiters = 0;
  done = 0;
  ok = true;
  for (n=0; n < THREADS; n++) {
    results[n] = n;
    if (createOSThread(&ids[n], "test", go, (void*)&results[n])) {
      printf("unable to create thread %d\n", n);
      exit(1);
    }
  }
  OS_ACQUIRE_LOCK(&mutex);
  while(done != THREADS) {
    waitCondition(&cond, &mutex);
  }
  OS_RELEASE_LOCK(&mutex);
  for (n = 0; n < THREADS; n++) {
    if (results[n] != OK) {
      printf("%d: unexpected result was %d\n", n, results[n]);
      ok = false;
    }
  }
  hs_exit();
  return ok ? 0 : 1;
}

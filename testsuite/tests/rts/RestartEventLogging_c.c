#include <stdio.h>
#include <Rts.h>
#include <rts/EventLogFormat.h>

#define STOPPED       0
#define STARTED       1
#define WRITTEN       2

static int32_t state = STOPPED;

void test_init(void) {
  if (state != STOPPED) {
    printf("test_init was not called first or directly after test_stop\n");
  }

  state = STARTED;
  printf("init\n");
  fflush(stdout);
}

bool test_write(void *eventlog, size_t eventlog_size) {
  if (state == STOPPED) {
    printf("test_init was not called\n");
  }
  if (state == STARTED) {
    // Note that the encoding of the header is coppied from EventLog.c (see `postInt32()`)
    StgWord8 * words = (StgWord8 *)eventlog;
    StgInt32 h32 = EVENT_HEADER_BEGIN;
    StgWord32 h = (StgWord32)h32; // Yes, the cast is correct. See `postInt32()`
    if ((words[0] != (StgWord8)(h >> 24))
        || (words[1] != (StgWord8)(h >> 16))
        || (words[2] != (StgWord8)(h >> 8))
        || (words[3] != (StgWord8)h)) {
      printf("ERROR: event does not start with EVENT_HEADER_BEGIN\n");
      printf("0x%x != 0x%x\n", words[0], (StgWord8)(h >> 24));
      printf("0x%x != 0x%x\n", words[1], (StgWord8)(h >> 16));
      printf("0x%x != 0x%x\n", words[2], (StgWord8)(h >> 8));
      printf("0x%x != 0x%x\n", words[3], (StgWord8)h);
    }
    else {
      printf("Event log started with EVENT_HEADER_BEGIN\n");
    }
  }

  state = WRITTEN;

  printf("write\n");
  fflush(stdout);
  return true;
}

void test_flush(void) {
  printf("flush\n");
  fflush(stdout);
}

void test_stop(void) {
  state = STOPPED;
  printf("stop\n");
  fflush(stdout);
}

const EventLogWriter writer = {
  .initEventLogWriter = test_init,
  .writeEventLog = test_write,
  .flushEventLog = test_flush,
  .stopEventLogWriter = test_stop
};

void c_restart_eventlog(void) {
  for (int i = 0; i < 10; i++) {
    if (!startEventLogging(&writer)) {
      printf("failed to start eventlog\n");
    }
    endEventLogging();
  }
}


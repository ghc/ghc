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
    // Note that the order of bytes is reversed compared with EVENT_HEADER_BEGIN
    int32_t header = *((int32_t *)eventlog);
    if (header != 0x62726468) {
      printf("ERROR: event does not start with EVENT_HEADER_BEGIN\n");
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
  for (int i = 0; i < 3; i++) {
    if (!startEventLogging(&writer)) {
      printf("failed to start eventlog\n");
    }
    endEventLogging();
  }
}


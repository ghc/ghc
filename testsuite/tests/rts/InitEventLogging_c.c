#include <stdio.h>
#include <Rts.h>

void test_init(void) {
  printf("init\n");
  fflush(stdout);
}

bool test_write(void *eventlog, size_t eventlog_size) {
  printf("write\n");
  fflush(stdout);
  return true;
}

void test_flush(void) {
  printf("flush\n");
  fflush(stdout);
}

void test_stop(void) {
  printf("stop\n");
  fflush(stdout);
}

const EventLogWriter writer = {
  .initEventLogWriter = test_init,
  .writeEventLog = test_write,
  .flushEventLog = test_flush,
  .stopEventLogWriter = test_stop
};

void init_eventlog(void) {
  if (!startEventLogging(&writer)) {
    printf("failed to start eventlog\n");
  }
}


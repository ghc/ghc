#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>

typedef int stat_func(const char*, struct stat*);

stat_func *foo = &stat;

void stat_test(void)
{
  struct stat buf;

  printf("About to stat-test.c\n");
  foo("stat-test.c", &buf);
  printf("Done\n");
}

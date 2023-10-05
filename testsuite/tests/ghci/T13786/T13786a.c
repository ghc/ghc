#include <stdio.h>
#include <stdbool.h>

static bool flag_a = false;

extern void hello_b();

void hello_a() {
  if (! flag_a) {
      flag_a = true;
      hello_b();
  }

  printf("hello world A\n");
}

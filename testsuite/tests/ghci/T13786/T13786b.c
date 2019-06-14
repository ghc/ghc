#include <stdio.h>
#include <stdbool.h>

static bool flag_b = false;

extern void hello_a();

void hello_b() {
  if (! flag_b) {
      flag_b = true;
      hello_a();
  }

  printf("hello world B\n");
}


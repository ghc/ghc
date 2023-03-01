#include "ffi002_stub.h"
#include "HsFFI.h"

#include <stdio.h>

int main(int argc, char *argv[])
{
  int i;

  hs_init(&argc, &argv);

  for (i = 0; i < 5; i++) {
    printf("%ld\n", foo(2500));
  }

  hs_exit();

  return 0;
}

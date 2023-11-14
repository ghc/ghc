#include <stdio.h>
#include <stdbool.h>
#include <HsFFI.h>
#include "F016_stub.h"

int main (int argc, char *argv[])
{
  hs_init(&argc, &argv);
  HsInt y;
  y = f(5);
  printf("Result %ld", y);
  hs_exit();
  return 0;
}



#include <stdio.h>
#include "T7040_c.h"

int x = 0;

void printx() {
  printf("x: %d\n", x);
  x = 1;
  printf("x: %d\n", x);
}


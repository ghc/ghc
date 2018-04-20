#include <stdio.h>

void hello(void (*f)()) {
  printf("in C\n");
  f();
}

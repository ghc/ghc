#include "T23034.h"
#include <stdio.h>

void t_printf(signed long a, signed int b, signed short c, signed char d) {
  printf("%ld %ld %ld %ld\n", a, 0L + b, 0L + c, 0L + d);
}

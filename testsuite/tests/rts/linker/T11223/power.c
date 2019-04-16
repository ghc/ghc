#include <stdio.h>

#if defined(WEAK)
int power2(int x) __attribute__((weak));
#endif
int power2(int x)
{
  fprintf(stderr, "fast power2()\n");
  return x*x;
}

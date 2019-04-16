#include <stdio.h>
#if defined(WEAK)
int power2(int x) __attribute__((weak));
#endif
int power2(int x)
{
  fprintf(stderr, "slow power2()\n");
  return x*x;
}

int power3(int x)
{
  return power2(x)*x;
}

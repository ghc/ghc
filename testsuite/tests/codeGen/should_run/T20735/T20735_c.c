#include <stdio.h>
#include <stdint.h>


int64_t func64(int64_t targetType)
{
  printf("%d\n", (int)targetType);
  fflush(stdout);
  return (-1);
}

int32_t func32(int32_t targetType)
{
  printf("%d\n", (int)targetType);
  fflush(stdout);
  return (-1);
}

int16_t func16(int16_t targetType)
{
  printf("%d\n", (int)targetType);
  fflush(stdout);
  return (-1);
}

int8_t func8(int8_t targetType)
{
  printf("%d\n", (int)targetType);
  fflush(stdout);
  return (-1);
}


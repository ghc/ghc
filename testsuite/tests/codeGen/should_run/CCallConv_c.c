#include "stdint.h"
#include "stdio.h"

int64_t fun8(int8_t a0, uint8_t a1, int8_t a2, int8_t a3, int8_t a4, int8_t a5,
             int8_t a6, int8_t a7, int8_t s0, uint8_t s1) {
  printf("fun8:\n");
  printf("a0: %#x %hhd\n", a0, a0);
  printf("a1: %#x %hhu\n", a1, a1);
  printf("a2: %#x %hhd\n", a2, a2);
  printf("a3: %#x %hhd\n", a3, a3);
  printf("a4: %#x %hhd\n", a4, a4);
  printf("a5: %#x %hhd\n", a5, a5);
  printf("a6: %#x %hhd\n", a6, a6);
  printf("a7: %#x %hhd\n", a7, a7);
  printf("s0: %#x %hhd\n", s0, s0);
  printf("s1: %#x %hhu\n", s1, s1);

  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + s0 + s1;
}

int64_t fun16(int16_t a0, uint16_t a1, int16_t a2, int16_t a3, int16_t a4,
              int16_t a5, int16_t a6, int16_t a7, int16_t s0, uint16_t s1) {
  printf("fun16:\n");
  printf("a0: %#x %hd\n", a0, a0);
  printf("a1: %#x %hu\n", a1, a1);
  printf("a2: %#x %hd\n", a2, a2);
  printf("a3: %#x %hd\n", a3, a3);
  printf("a4: %#x %hd\n", a4, a4);
  printf("a5: %#x %hd\n", a5, a5);
  printf("a6: %#x %hd\n", a6, a6);
  printf("a7: %#x %hd\n", a7, a7);
  printf("s0: %#x %hd\n", s0, s0);
  printf("s1: %#x %hu\n", s1, s1);

  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + s0 + s1;
}

int64_t fun32(int32_t a0, uint32_t a1, int32_t a2, int32_t a3, int32_t a4,
              int32_t a5, int32_t a6, int32_t a7, int32_t s0, uint32_t s1) {
  printf("fun32:\n");
  printf("a0: %#x %d\n", a0, a0);
  printf("a1: %#x %u\n", a1, a1);
  printf("a2: %#x %d\n", a2, a2);
  printf("a3: %#x %d\n", a3, a3);
  printf("a4: %#x %d\n", a4, a4);
  printf("a5: %#x %d\n", a5, a5);
  printf("a6: %#x %d\n", a6, a6);
  printf("a7: %#x %d\n", a7, a7);
  printf("s0: %#x %d\n", s0, s0);
  printf("s1: %#x %u\n", s1, s1);

  // Ensure the addition happens in long int (not just int) precission.
  // Otherwise, the result is truncated during the operation.
  int64_t force_int64_precission = 0;
  return force_int64_precission + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + s0 +
         s1;
}

#include <stdint.h>
#include <stdio.h>

int64_t fun8(int8_t a0, uint8_t a1, int8_t a2, int8_t a3, int8_t a4, int8_t a5,
             int8_t a6, int8_t a7, uint8_t s0, int8_t s1) {
  printf("fun8:\n");
  printf("a0: %#x %hhd\n", a0, a0);
  printf("a1: %#x %hhu\n", a1, a1);
  printf("a2: %#x %hhd\n", a2, a2);
  printf("a3: %#x %hhd\n", a3, a3);
  printf("a4: %#x %hhd\n", a4, a4);
  printf("a5: %#x %hhd\n", a5, a5);
  printf("a6: %#x %hhd\n", a6, a6);
  printf("a7: %#x %hhd\n", a7, a7);
  printf("s0: %#x %hhu\n", s0, s0);
  printf("s1: %#x %hhd\n", s1, s1);

  fflush(stdout);

  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + s0 + s1;
}

int64_t fun16(int16_t a0, uint16_t a1, int16_t a2, int16_t a3, int16_t a4,
              int16_t a5, int16_t a6, int16_t a7, uint16_t s0, int16_t s1) {
  printf("fun16:\n");
  printf("a0: %#x %hd\n", a0, a0);
  printf("a1: %#x %hu\n", a1, a1);
  printf("a2: %#x %hd\n", a2, a2);
  printf("a3: %#x %hd\n", a3, a3);
  printf("a4: %#x %hd\n", a4, a4);
  printf("a5: %#x %hd\n", a5, a5);
  printf("a6: %#x %hd\n", a6, a6);
  printf("a7: %#x %hd\n", a7, a7);
  printf("s0: %#x %hu\n", s0, s0);
  printf("s1: %#x %hd\n", s1, s1);

  fflush(stdout);

  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + s0 + s1;
}

int64_t fun32(int32_t a0, uint32_t a1, int32_t a2, int32_t a3, int32_t a4,
              int32_t a5, int32_t a6, int32_t a7, uint32_t s0, int32_t s1) {
  printf("fun32:\n");
  printf("a0: %#x %d\n", a0, a0);
  printf("a1: %#x %u\n", a1, a1);
  printf("a2: %#x %d\n", a2, a2);
  printf("a3: %#x %d\n", a3, a3);
  printf("a4: %#x %d\n", a4, a4);
  printf("a5: %#x %d\n", a5, a5);
  printf("a6: %#x %d\n", a6, a6);
  printf("a7: %#x %d\n", a7, a7);
  printf("s0: %#x %u\n", s0, s0);
  printf("s1: %#x %d\n", s1, s1);

  fflush(stdout);

  // Ensure the addition happens in long int (not just int) precission.
  // Otherwise, the result is truncated during the operation.
  int64_t force_int64_precission = 0;
  return force_int64_precission + a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + s0 +
         s1;
}

float funFloat(float a0, float a1, float a2, float a3, float a4, float a5,
             float a6, float a7, float s0, float s1) {
  printf("funFloat:\n");
  printf("a0: %f\n", a0);
  printf("a1: %f\n", a1);
  printf("a2: %f\n", a2);
  printf("a3: %f\n", a3);
  printf("a4: %f\n", a4);
  printf("a5: %f\n", a5);
  printf("a6: %f\n", a6);
  printf("a7: %f\n", a7);
  printf("s0: %f\n", s0);
  printf("s1: %f\n", s1);

  fflush(stdout);

  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + s0 + s1;
}

double funDouble(double a0, double a1, double a2, double a3, double a4, double a5,
             double a6, double a7, double s0, double s1) {
  printf("funDouble:\n");
  printf("a0: %f\n", a0);
  printf("a1: %f\n", a1);
  printf("a2: %f\n", a2);
  printf("a3: %f\n", a3);
  printf("a4: %f\n", a4);
  printf("a5: %f\n", a5);
  printf("a6: %f\n", a6);
  printf("a7: %f\n", a7);
  printf("s0: %f\n", s0);
  printf("s1: %f\n", s1);

  fflush(stdout);

  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + s0 + s1;
}

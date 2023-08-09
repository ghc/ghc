#include "stdint.h"
#include "stdio.h"

int8_t i8_i8i8i8i8i8i8i8i8i8(int8_t a0, int8_t a1, int8_t a2, int8_t a3,
                           int8_t a4, int8_t a5, int8_t a6, int8_t a7, int8_t a8) {
  printf("i8_i8i8i8i8i8i8i8i8i8\n");
  printf("a0: %hhd\n", a0);
  printf("a1: %hhd\n", a1);
  printf("a2: %hhd\n", a2);
  printf("a3: %hhd\n", a3);
  printf("a4: %hhd\n", a4);
  printf("a5: %hhd\n", a5);
  printf("a6: %hhd\n", a6);
  printf("a7: %hhd\n", a7);
  printf("a8: %hhd\n", a8);

  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
}

int16_t i16_i16i16i16i16i16i16i16i16i16(int16_t a0, int16_t a1, int16_t a2, int16_t a3,
                           int16_t a4, int16_t a5, int16_t a6, int16_t a7, int16_t a8) {
  printf("i16_i16i16i16i16i16i16i16i16i16\n");
  printf("a0: %hd\n", a0);
  printf("a1: %hd\n", a1);
  printf("a2: %hd\n", a2);
  printf("a3: %hd\n", a3);
  printf("a4: %hd\n", a4);
  printf("a5: %hd\n", a5);
  printf("a6: %hd\n", a6);
  printf("a7: %hd\n", a7);
  printf("a8: %hd\n", a8);

  return a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
}

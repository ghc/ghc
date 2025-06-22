#include "riscv_vector.h"
#include <stdio.h>

static void printVec_int64(vint64m1_t v, int length) {
  // Extract and print elements from the vector register
  int64_t temp[length]; // Temporary array to hold vector elements
  __riscv_vse64_v_i64m1(temp, v, length); // Store vector to memory

  printf("[%ld", temp[0]);
  for (int i = 1; i < length; i++) {
    printf(", %ld", temp[i]);
  }
  printf("]\n");
  fflush(stdout);
}
// Provide many vectors to enforce stack usage
void printVecs_int64x2_c(vint64m1_t v8, vint64m1_t v9, vint64m1_t v10,
                         vint64m1_t v11, vint64m1_t v12, vint64m1_t v13,
                         vint64m1_t v14, vint64m1_t v15, vint64m1_t v16,
                         vint64m1_t v17, vint64m1_t v18, vint64m1_t v19,
                         vint64m1_t v20, vint64m1_t v21, vint64m1_t v22,
                         vint64m1_t v23) {
  printVec_int64(v8, 2);
  printVec_int64(v9, 2);
  printVec_int64(v10, 2);
  printVec_int64(v11, 2);
  printVec_int64(v12, 2);
  printVec_int64(v13, 2);
  printVec_int64(v14, 2);
  printVec_int64(v15, 2);
  printVec_int64(v16, 2);
  printVec_int64(v17, 2);
  printVec_int64(v18, 2);
  printVec_int64(v19, 2);
  printVec_int64(v20, 2);
  printVec_int64(v21, 2);
  printVec_int64(v22, 2);
  printVec_int64(v23, 2);

  fflush(stdout);
}

vint64m1_t return_int64X2() {
  int64_t v[] = {INT64_MIN, INT64_MAX};
  return __riscv_vle64_v_i64m1(v, 2);
}

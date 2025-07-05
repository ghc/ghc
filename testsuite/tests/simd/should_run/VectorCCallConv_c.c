#include "riscv_vector.h"
#include <float.h>
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

static void printVec_double(vfloat64m1_t v, int length) {
  // Extract and print elements from the vector register
  double temp[length]; // Temporary array to hold vector elements
  __riscv_vse64_v_f64m1(temp, v, length); // Store vector to memory

  printf("[%f", temp[0]);
  for (int i = 1; i < length; i++) {
    printf(", %f", temp[i]);
  }
  printf("]\n");
  fflush(stdout);
}
// Provide many vectors to enforce stack usage
void printVecs_doublex2_c(vfloat64m1_t v8, vfloat64m1_t v9, vfloat64m1_t v10,
                          vfloat64m1_t v11, vfloat64m1_t v12, vfloat64m1_t v13,
                          vfloat64m1_t v14, vfloat64m1_t v15, vfloat64m1_t v16,
                          vfloat64m1_t v17, vfloat64m1_t v18, vfloat64m1_t v19,
                          vfloat64m1_t v20, vfloat64m1_t v21, vfloat64m1_t v22,
                          vfloat64m1_t v23) {
  printVec_double(v8, 2);
  printVec_double(v9, 2);
  printVec_double(v10, 2);
  printVec_double(v11, 2);
  printVec_double(v12, 2);
  printVec_double(v13, 2);
  printVec_double(v14, 2);
  printVec_double(v15, 2);
  printVec_double(v16, 2);
  printVec_double(v17, 2);
  printVec_double(v18, 2);
  printVec_double(v19, 2);
  printVec_double(v20, 2);
  printVec_double(v21, 2);
  printVec_double(v22, 2);
  printVec_double(v23, 2);

  fflush(stdout);
}

vfloat64m1_t return_doubleX2() {
  double v[] = {DBL_MIN, DBL_MAX};
  return __riscv_vle64_v_f64m1(v, 2);
}

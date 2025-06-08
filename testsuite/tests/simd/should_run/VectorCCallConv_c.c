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
}
void printVecs_int64x2(vint64m1_t v1, vint64m1_t v2, vint64m1_t v3,
                       vint64m1_t v4, vint64m1_t v5, vint64m1_t v6,
                       vint64m1_t v7, vint64m1_t v8, vint64m1_t v9,
                       vint64m1_t v10, vint64m1_t v11, vint64m1_t v12,
                       vint64m1_t v13, vint64m1_t v14, vint64m1_t v15,
                       vint64m1_t v16, vint64m1_t v17, vint64m1_t v18,
                       vint64m1_t v19, vint64m1_t v20, vint64m1_t v21,
                       vint64m1_t v22, vint64m1_t v23, vint64m1_t v24,
                       vint64m1_t v25, vint64m1_t v26, vint64m1_t v27,
                       vint64m1_t v28, vint64m1_t v29, vint64m1_t v30,
                       vint64m1_t v31, vint64m1_t v32, vint64m1_t v33,
                       vint64m1_t v34, vint64m1_t v35, vint64m1_t v36);

// Provide many vectors to enforce stack usage
void printVecs_int64x2(vint64m1_t v1, vint64m1_t v2, vint64m1_t v3,
                       vint64m1_t v4, vint64m1_t v5, vint64m1_t v6,
                       vint64m1_t v7, vint64m1_t v8, vint64m1_t v9,
                       vint64m1_t v10, vint64m1_t v11, vint64m1_t v12,
                       vint64m1_t v13, vint64m1_t v14, vint64m1_t v15,
                       vint64m1_t v16, vint64m1_t v17, vint64m1_t v18,
                       vint64m1_t v19, vint64m1_t v20, vint64m1_t v21,
                       vint64m1_t v22, vint64m1_t v23, vint64m1_t v24,
                       vint64m1_t v25, vint64m1_t v26, vint64m1_t v27,
                       vint64m1_t v28, vint64m1_t v29, vint64m1_t v30,
                       vint64m1_t v31, vint64m1_t v32, vint64m1_t v33,
                       vint64m1_t v34, vint64m1_t v35, vint64m1_t v36) {
  printVec_int64(v1, 2);
  printVec_int64(v2, 2);
  printVec_int64(v3, 2);
  printVec_int64(v4, 2);
  printVec_int64(v5, 2);
  printVec_int64(v6, 2);
  printVec_int64(v7, 2);
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
  printVec_int64(v24, 2);
  printVec_int64(v25, 2);
  printVec_int64(v26, 2);
  printVec_int64(v27, 2);
  printVec_int64(v28, 2);
  printVec_int64(v29, 2);
  printVec_int64(v30, 2);
  printVec_int64(v31, 2);
  printVec_int64(v32, 2);
  printVec_int64(v33, 2);
  printVec_int64(v34, 2);
  printVec_int64(v35, 2);
  printVec_int64(v36, 2);

  fflush(stdout);
}

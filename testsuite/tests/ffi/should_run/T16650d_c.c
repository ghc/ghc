#include <stdint.h>

// See T16650b_c.c for commentary.
uint8_t is_doubleton_homogenous (void **arr) {
  return (arr[0] == arr[1]);
}


#include <stdint.h>

// Take the first element of a byte array. The array
// must have length >= 1.
uint8_t head_bytearray (uint8_t *arr) {
  return arr[0];
}

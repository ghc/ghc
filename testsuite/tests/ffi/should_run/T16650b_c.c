#include <stdint.h>

// Check to see if the first two elements in the array are
// the same pointer. Technically, GHC only promises that this is
// deterministic for arrays of unlifted identity-supporting
// types (MutableByteArray#, TVar#, MutVar#, etc.). However,
// in the tests, we assume that even for types that do not
// support identity (all lifted types, ByteArray#, Array#, etc.),
// GHC initializes every element in an array to the same pointer
// with newArray#. This is the GHC's actual behavior, and if
// newArray# stopped behaving this way, even if it wouldn't
// be a semantic bug, it would be a performance bug. Consequently,
// we assume this behavior in tests T16650c and T16650d.
uint8_t is_doubleton_homogenous (void **arr) {
  return (arr[0] == arr[1]);
}


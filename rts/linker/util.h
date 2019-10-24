#pragma once

#include <stdint.h>
#include <stdbool.h>
#include "LinkerInternals.h"

// Signed extend a number to a 32-bit int.
// Does the given signed integer fit into the given bit width?
static inline int32_t
signExtend32(uint32_t bits, uint32_t x)
{
    return ((int32_t) (x << (32 - bits))) >> (32 - bits);
}

// Does the given signed integer fit into the given bit width?
static inline bool
isInt(uint32_t bits, int32_t x)
{
    return bits > 32 || (-(1 << (bits-1)) <= x
                         && x < (1 << (bits-1)));
}

static inline bool
isInt64(uint32_t bits, int64_t x) {
    return bits > 64 || (-((int64_t)1 << (bits-1)) <= x
                         && x < ((int64_t)1 << (bits-1)));
}

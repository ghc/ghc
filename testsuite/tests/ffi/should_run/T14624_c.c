#include <stdint.h>
#include <math.h>
#include "T14624_c.h"
#include "Rts.h"

StgChar f_char(StgChar c) {
    return c;
}

StgInt f_int(StgInt a) {
    return a - STG_INT_MAX;
}

StgInt8 f_int8(StgInt8 a) {
    return a - STG_INT8_MAX;
}

StgInt16 f_int16(StgInt16 a) {
    return a - STG_INT16_MAX;
}

StgInt32 f_int32(StgInt32 a) {
    return a - STG_INT32_MAX;
}

StgInt64 f_int64(StgInt64 a) {
    return a - STG_INT64_MAX;
}

StgWord f_word(StgWord a) {
    return a - STG_WORD_MAX;
}

StgWord8 f_word8(StgWord8 a) {
    return a - STG_WORD8_MAX;
}

StgWord16 f_word16(StgWord16 a) {
    return a - STG_WORD16_MAX;
}

StgWord32 f_word32(StgWord32 a) {
    return a - STG_WORD32_MAX;
}

StgWord64 f_word64(StgWord64 a) {
    return a - STG_WORD64_MAX;
}

StgFloat f_float(StgFloat a) {
    StgFloat a1 = a;
    return (a / a1) - 1;
}

StgDouble f_double(StgDouble a, StgDouble b) {
    return fmod(a, b);
}

void* f_addr(void* a) {
    return a;
}

void* f_stable_ptr(void* a) {
   return a;
}

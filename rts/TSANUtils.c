#include <Rts.h>

#if defined(TSAN_ENABLED)

uint64_t ghc_tsan_atomic64_compare_exchange(uint64_t *ptr, uint64_t expected, uint64_t new_value, int success_memorder, int failure_memorder)
{
    __tsan_atomic64_compare_exchange_strong(
            ptr, &expected, new_value,
            success_memorder, failure_memorder);
    return expected;
}

uint32_t ghc_tsan_atomic32_compare_exchange(uint32_t *ptr, uint32_t expected, uint32_t new_value, int success_memorder, int failure_memorder)
{
    __tsan_atomic32_compare_exchange_strong(
            ptr, &expected, new_value,
            success_memorder, failure_memorder);
    return expected;
}

uint16_t ghc_tsan_atomic16_compare_exchange(uint16_t *ptr, uint16_t expected, uint16_t new_value, int success_memorder, int failure_memorder)
{
    __tsan_atomic16_compare_exchange_strong(
            ptr, &expected, new_value,
            success_memorder, failure_memorder);
    return expected;
}

uint8_t ghc_tsan_atomic8_compare_exchange(uint8_t *ptr, uint8_t expected, uint8_t new_value, int success_memorder, int failure_memorder)
{
    __tsan_atomic8_compare_exchange_strong(
            ptr, &expected, new_value,
            success_memorder, failure_memorder);
    return expected;
}

#endif

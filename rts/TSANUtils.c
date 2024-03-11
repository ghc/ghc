#include <Rts.h>

#if defined(TSAN_ENABLED)

__tsan_atomic64 ghc_tsan_atomic64_compare_exchange(volatile __tsan_atomic64 *ptr, __tsan_atomic64 expected, __tsan_atomic64 new_value, int success_memorder, int failure_memorder)
{
    __tsan_atomic64_compare_exchange_strong(
            ptr, &expected, new_value,
            success_memorder, failure_memorder);
    return expected;
}

__tsan_atomic32 ghc_tsan_atomic32_compare_exchange(volatile __tsan_atomic32 *ptr, __tsan_atomic32 expected, __tsan_atomic32 new_value, int success_memorder, int failure_memorder)
{
    __tsan_atomic32_compare_exchange_strong(
            ptr, &expected, new_value,
            success_memorder, failure_memorder);
    return expected;
}

__tsan_atomic16 ghc_tsan_atomic16_compare_exchange(volatile __tsan_atomic16 *ptr, __tsan_atomic16 expected, __tsan_atomic16 new_value, int success_memorder, int failure_memorder)
{
    __tsan_atomic16_compare_exchange_strong(
            ptr, &expected, new_value,
            success_memorder, failure_memorder);
    return expected;
}

__tsan_atomic8 ghc_tsan_atomic8_compare_exchange(volatile __tsan_atomic8 *ptr, __tsan_atomic8 expected, __tsan_atomic8 new_value, int success_memorder, int failure_memorder)
{
    __tsan_atomic8_compare_exchange_strong(
            ptr, &expected, new_value,
            success_memorder, failure_memorder);
    return expected;
}

#endif

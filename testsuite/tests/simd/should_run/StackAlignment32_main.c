#include "HsFFI.h"
#include <immintrin.h>

extern void foo(double x);

__m256d add10(__m256d a0, __m256d a1, __m256d a2, __m256d a3, __m256d a4, __m256d a5, __m256d a6, __m256d a7, __m256d a8, __m256d a9)
{
    // Test whether the stack is 32-byte aligned.
    // Under the System V ABI, the first eight parameters are passed in registers.
    // Here, `a8` and `a9` are passed on the stack.
    // We want the compiler to emit `vmovapd` so we can verify 32-byte stack alignment.
    // We cannot use `_mm256_add_pd(a0, a9)` because the compiler may fuse the load and
    // the addition and emit a `vaddpd m256` form, which permits an unaligned load of `a9`.
    // Therefore, we ensure that both operands are read from the stack,
    // so that at least one operand is loaded using `vmovapd`.
    return _mm256_add_pd(a8, a9);
}

__attribute__((noinline))
void baz(void)
{
    // Make the stack pointer shift by 16 bytes on x86-64
    foo(2.2);
}

__attribute__((noinline))
void bar(void)
{
    // Make the stack pointer shift by 16 bytes on x86-64
    foo(1.1);
    baz();
}

int main(int argc, char *argv[])
{
    hs_init(&argc, &argv);

    foo(0.1);
    bar();

    hs_exit();
}

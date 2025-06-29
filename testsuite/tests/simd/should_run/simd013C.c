#if defined(__x86_64__) || defined(_M_X64)
#include <immintrin.h>

__m128d sub(__m128d x, __m128d y)
{
  return _mm_sub_pd(x,y);
}

__m128d add7(__m128d x1, __m128d x2, __m128d x3, __m128d x4, __m128d x5, __m128d x6, __m128d x7)
{
  return _mm_add_pd(x1,_mm_add_pd(x2,_mm_add_pd(x3,_mm_add_pd(x4,_mm_add_pd(x5,_mm_add_pd(x6, x7))))));
}
#elif defined(__riscv_v)
#if __riscv_v_intrinsic >= 12000
#include <riscv_vector.h>

size_t vl = 2;

vfloat64m1_t sub(vfloat64m1_t x, vfloat64m1_t y)
{
  return __riscv_vfsub(x,y,vl);
}

vfloat64m1_t add7(vfloat64m1_t x1, vfloat64m1_t x2, vfloat64m1_t x3, vfloat64m1_t x4, vfloat64m1_t x5, vfloat64m1_t x6, vfloat64m1_t x7)
{
  return __riscv_vfadd(x1,__riscv_vfadd(x2,__riscv_vfadd(x3,__riscv_vfadd(x4,__riscv_vfadd(x5,__riscv_vfadd(x6, x7, vl),vl),vl),vl),vl),vl);
}
#else
#error "RISC-V vector target, but current intrinsics not supported."
#endif
#endif

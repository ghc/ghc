#include <stdio.h>
#include <xmmintrin.h>
#include <float.h>

static unsigned int
getFPUStateX86 (void)
{
    unsigned int control = 0;
#if defined(_MSC_VER)
    control = _controlfp(0, 0);
#else
    __asm__ __volatile__("fnstcw %0" : "=m" (control));
#endif
    return control;
}

static unsigned int
getSSEStateX86 (void)
{
    return _mm_getcsr();
}

extern void showControlBits (void)
{
    printf("FPU: 0x%04x\n", getFPUStateX86());
}

#include <stdio.h>
#include "CheckVectorSupport.h"

// Check support for vector registers (conservative).
//
// 0: no support for vector registers
// 1: support for 128-bit vector registers
// 2: support for 256-bit vector registers
// 3: support for 512-bit vector registers
int checkVectorSupport(void) {

    int supports_V16;
    int supports_V32;
    int supports_V64;

  // Detect x86/x86_64 support using CPUID
  #if defined(__x86_64__) || defined(_M_X64) || defined(__i386) || defined(_M_IX86)
    int eax, ebx, ecx, edx;

    // Check for SSE (128-bit)
    eax = 1;
    __asm__ __volatile__ (
        "cpuid"
        : "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx)
        : "a" (eax)
    );

    // SSE
    supports_V16 = edx & (1 << 25);

    // Check for AVX2 and AVX512-F
    eax = 7;
    ecx = 0;
    __asm__ __volatile__ (
        "cpuid"
        : "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx)
        : "a" (eax), "c" (ecx)
    );

    // AVX2
    supports_V32 = ebx & (1 << 5);

    // AVX-512F
    supports_V64 = ebx & (1 << 16);

  // Detect AArch64 support
  #elif defined(__aarch64__)
    // AArch64 supports NEON (128-bit vectors) by default.
    supports_V16 = 1;
    // We don't support 256/512 bit vectors on AArch64 for now.
    supports_V32 = 0;
    supports_V64 = 0;

  // Detect PowerPC support
  #elif defined(__powerpc__) || defined(__ppc__) || defined(__PPC__)
    // For now, we don't support vectors on PowerPC.
    supports_V16 = 0;
    supports_V32 = 0;
    supports_V64 = 0;
/*
    unsigned long hwcap = 0;
    hwcap = getauxval(AT_HWCAP);  // Get hardware capabilities on Linux
    supports_V16 = hwcap & PPC_FEATURE_HAS_ALTIVEC;
    supports_V32 = hwcap & PPC_FEATURE_HAS_VSX;
*/

  #elif defined(__riscv)
// csrr instruction nott allowed in user-mode qemu emulation of riscv
// Backend doesn't yet support vector registers, so hard-coded to no vector support
// for now.
//
//    unsigned long vlenb;
//    asm volatile ("csrr %0, vlenb" : "=r" (vlenb));
    // VLENB gives the length in bytes
    supports_V16 = 0;
    supports_V32 = 0;
    supports_V64 = 0;

  #else
    // On other platforms, we conservatively return no vector support.
    supports_V16 = 0;
    supports_V32 = 0;
    supports_V64 = 0;
  #endif

    if (supports_V64) {
      return 3;
    }
    if (supports_V32) {
      return 2;
    }
    if (supports_V16) {
      return 1;
    }
    return 0;
}

int vectorSupportGlobalVar;
void setVectorSupport(void){
  vectorSupportGlobalVar = checkVectorSupport();
}

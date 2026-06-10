/* Host x86 CPU feature detection, used to implement -march=native.
 *
 * The detection code is vendored from LLVM's compiler-rt, where it implements
 * the runtime support for __builtin_cpu_supports/__builtin_cpu_is
 * (__cpu_model, __cpu_indicator_init):
 *
 *   compiler-rt/lib/builtins/cpu_model/x86.c
 *   at tag llvmorg-20.1.0 (LLVM 20.1.0)
 *   https://github.com/llvm/llvm-project/blob/llvmorg-20.1.0/compiler-rt/lib/builtins/cpu_model/x86.c
 *
 * The vendored code is part of the LLVM Project, under the Apache License v2.0
 * with LLVM Exceptions; see https://llvm.org/LICENSE.txt for license
 * information. Upstream notes that this file is itself a copy of
 * llvm/lib/TargetParser/Host.cpp -- the code behind clang's -march=native --
 * and that the two are kept in sync.
 *
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *
 * Vendored verbatim: enum ProcessorFeatures, getX86CpuIDAndInfo,
 * getX86CpuIDAndInfoEx, getX86XCR0 and getAvailableFeatures, the latter with
 * a single marked GHC deviation that additionally gates FEATURE_FMA on OS
 * support for saving the AVX register state.
 *
 * Adaptations around the vendored code:
 *
 *   - ghc_detect_x86_cpu_features stands in for __cpu_indicator_init as the
 *     driver: it performs the same CPUID call sequence, but returns the first
 *     64 feature bits as a mask -- which covers every feature GHC decodes,
 *     see GHC.Driver.CpuFeatures -- instead of filling in the __cpu_model
 *     globals.
 *   - On non-x86 hosts the driver compiles to a stub returning 0 instead of
 *     upstream's "#error This file is intended only for x86-based targets".
 */

#include <HsFFI.h>
#include <stdbool.h>

#if defined(__i386__) || defined(_M_IX86) || defined(__x86_64__) || \
    defined(_M_X64)
#define GHC_HOST_IS_X86 1
#endif

#if defined(GHC_HOST_IS_X86)

#if (defined(__GNUC__) || defined(__clang__)) && !defined(_MSC_VER)
#include <cpuid.h>
#endif

#ifdef _MSC_VER
#include <intrin.h>
#endif

/* NOTE: The feature bit positions below are LLVM/GCC's stable
 * __builtin_cpu_supports feature numbering. cpuFeatureBitLayout in
 * GHC.Driver.CpuFeatures must use the same values. */
enum ProcessorFeatures {
  FEATURE_CMOV = 0,
  FEATURE_MMX,
  FEATURE_POPCNT,
  FEATURE_SSE,
  FEATURE_SSE2,
  FEATURE_SSE3,
  FEATURE_SSSE3,
  FEATURE_SSE4_1,
  FEATURE_SSE4_2,
  FEATURE_AVX,
  FEATURE_AVX2,
  FEATURE_SSE4_A,
  FEATURE_FMA4,
  FEATURE_XOP,
  FEATURE_FMA,
  FEATURE_AVX512F,
  FEATURE_BMI,
  FEATURE_BMI2,
  FEATURE_AES,
  FEATURE_PCLMUL,
  FEATURE_AVX512VL,
  FEATURE_AVX512BW,
  FEATURE_AVX512DQ,
  FEATURE_AVX512CD,
  FEATURE_AVX512ER,
  FEATURE_AVX512PF,
  FEATURE_AVX512VBMI,
  FEATURE_AVX512IFMA,
  FEATURE_AVX5124VNNIW,
  FEATURE_AVX5124FMAPS,
  FEATURE_AVX512VPOPCNTDQ,
  FEATURE_AVX512VBMI2,
  FEATURE_GFNI,
  FEATURE_VPCLMULQDQ,
  FEATURE_AVX512VNNI,
  FEATURE_AVX512BITALG,
  FEATURE_AVX512BF16,
  FEATURE_AVX512VP2INTERSECT,
  // FIXME: Below Features has some missings comparing to gcc, it's because gcc
  // has some not one-to-one mapped in llvm.
  // FEATURE_3DNOW,
  // FEATURE_3DNOWP,
  FEATURE_ADX = 40,
  // FEATURE_ABM,
  FEATURE_CLDEMOTE = 42,
  FEATURE_CLFLUSHOPT,
  FEATURE_CLWB,
  FEATURE_CLZERO,
  FEATURE_CMPXCHG16B,
  // FIXME: Not adding FEATURE_CMPXCHG8B is a workaround to make 'generic' as
  // a cpu string with no X86_FEATURE_COMPAT features, which is required in
  // current implementantion of cpu_specific/cpu_dispatch FMV feature.
  // FEATURE_CMPXCHG8B,
  FEATURE_ENQCMD = 48,
  FEATURE_F16C,
  FEATURE_FSGSBASE,
  // FEATURE_FXSAVE,
  // FEATURE_HLE,
  // FEATURE_IBT,
  FEATURE_LAHF_LM = 54,
  FEATURE_LM,
  FEATURE_LWP,
  FEATURE_LZCNT,
  FEATURE_MOVBE,
  FEATURE_MOVDIR64B,
  FEATURE_MOVDIRI,
  FEATURE_MWAITX,
  // FEATURE_OSXSAVE,
  FEATURE_PCONFIG = 63,
  FEATURE_PKU,
  FEATURE_PREFETCHWT1,
  FEATURE_PRFCHW,
  FEATURE_PTWRITE,
  FEATURE_RDPID,
  FEATURE_RDRND,
  FEATURE_RDSEED,
  FEATURE_RTM,
  FEATURE_SERIALIZE,
  FEATURE_SGX,
  FEATURE_SHA,
  FEATURE_SHSTK,
  FEATURE_TBM,
  FEATURE_TSXLDTRK,
  FEATURE_VAES,
  FEATURE_WAITPKG,
  FEATURE_WBNOINVD,
  FEATURE_XSAVE,
  FEATURE_XSAVEC,
  FEATURE_XSAVEOPT,
  FEATURE_XSAVES,
  FEATURE_AMX_TILE,
  FEATURE_AMX_INT8,
  FEATURE_AMX_BF16,
  FEATURE_UINTR,
  FEATURE_HRESET,
  FEATURE_KL,
  // FEATURE_AESKLE,
  FEATURE_WIDEKL = 92,
  FEATURE_AVXVNNI,
  FEATURE_AVX512FP16,
  FEATURE_X86_64_BASELINE,
  FEATURE_X86_64_V2,
  FEATURE_X86_64_V3,
  FEATURE_X86_64_V4,
  FEATURE_AVXIFMA,
  FEATURE_AVXVNNIINT8,
  FEATURE_AVXNECONVERT,
  FEATURE_CMPCCXADD,
  FEATURE_AMX_FP16,
  FEATURE_PREFETCHI,
  FEATURE_RAOINT,
  FEATURE_AMX_COMPLEX,
  FEATURE_AVXVNNIINT16,
  FEATURE_SM3,
  FEATURE_SHA512,
  FEATURE_SM4,
  FEATURE_APXF,
  FEATURE_USERMSR,
  FEATURE_AVX10_1_256,
  FEATURE_AVX10_1_512,
  FEATURE_AVX10_2_256,
  FEATURE_AVX10_2_512,
  FEATURE_MOVRS,
  CPU_FEATURE_MAX
};

// This code is copied from lib/Support/Host.cpp.
// Changes to either file should be mirrored in the other.

/// getX86CpuIDAndInfo - Execute the specified cpuid and return the 4 values in
/// the specified arguments.  If we can't run cpuid on the host, return true.
static bool getX86CpuIDAndInfo(unsigned value, unsigned *rEAX, unsigned *rEBX,
                               unsigned *rECX, unsigned *rEDX) {
#if (defined(__GNUC__) || defined(__clang__)) && !defined(_MSC_VER)
  return !__get_cpuid(value, rEAX, rEBX, rECX, rEDX);
#elif defined(_MSC_VER)
  // The MSVC intrinsic is portable across x86 and x64.
  int registers[4];
  __cpuid(registers, value);
  *rEAX = registers[0];
  *rEBX = registers[1];
  *rECX = registers[2];
  *rEDX = registers[3];
  return false;
#else
  return true;
#endif
}

/// getX86CpuIDAndInfoEx - Execute the specified cpuid with subleaf and return
/// the 4 values in the specified arguments.  If we can't run cpuid on the host,
/// return true.
static bool getX86CpuIDAndInfoEx(unsigned value, unsigned subleaf,
                                 unsigned *rEAX, unsigned *rEBX, unsigned *rECX,
                                 unsigned *rEDX) {
  // TODO(boomanaiden154): When the minimum toolchain versions for gcc and clang
  // are such that __cpuidex is defined within cpuid.h for both, we can remove
  // the __get_cpuid_count function and share the MSVC implementation between
  // all three.
#if (defined(__GNUC__) || defined(__clang__)) && !defined(_MSC_VER)
  return !__get_cpuid_count(value, subleaf, rEAX, rEBX, rECX, rEDX);
#elif defined(_MSC_VER)
  int registers[4];
  __cpuidex(registers, value, subleaf);
  *rEAX = registers[0];
  *rEBX = registers[1];
  *rECX = registers[2];
  *rEDX = registers[3];
  return false;
#else
  return true;
#endif
}

// Read control register 0 (XCR0). Used to detect features such as AVX.
static bool getX86XCR0(unsigned *rEAX, unsigned *rEDX) {
  // TODO(boomanaiden154): When the minimum toolchain versions for gcc and clang
  // are such that _xgetbv is supported by both, we can unify the implementation
  // with MSVC and remove all inline assembly.
#if defined(__GNUC__) || defined(__clang__)
  // Check xgetbv; this uses a .byte sequence instead of the instruction
  // directly because older assemblers do not include support for xgetbv and
  // there is no easy way to conditionally compile based on the assembler used.
  __asm__(".byte 0x0f, 0x01, 0xd0" : "=a"(*rEAX), "=d"(*rEDX) : "c"(0));
  return false;
#elif defined(_MSC_FULL_VER) && defined(_XCR_XFEATURE_ENABLED_MASK)
  unsigned long long Result = _xgetbv(_XCR_XFEATURE_ENABLED_MASK);
  *rEAX = Result;
  *rEDX = Result >> 32;
  return false;
#else
  return true;
#endif
}

static void getAvailableFeatures(unsigned ECX, unsigned EDX, unsigned MaxLeaf,
                                 unsigned *Features) {
  unsigned EAX = 0, EBX = 0;

#define hasFeature(F) ((Features[F / 32] >> (F % 32)) & 1)
#define setFeature(F) Features[F / 32] |= 1U << (F % 32)

  if ((EDX >> 15) & 1)
    setFeature(FEATURE_CMOV);
  if ((EDX >> 23) & 1)
    setFeature(FEATURE_MMX);
  if ((EDX >> 25) & 1)
    setFeature(FEATURE_SSE);
  if ((EDX >> 26) & 1)
    setFeature(FEATURE_SSE2);

  if ((ECX >> 0) & 1)
    setFeature(FEATURE_SSE3);
  if ((ECX >> 1) & 1)
    setFeature(FEATURE_PCLMUL);
  if ((ECX >> 9) & 1)
    setFeature(FEATURE_SSSE3);
  if ((ECX >> 12) & 1)
    setFeature(FEATURE_FMA);
  if ((ECX >> 13) & 1)
    setFeature(FEATURE_CMPXCHG16B);
  if ((ECX >> 19) & 1)
    setFeature(FEATURE_SSE4_1);
  if ((ECX >> 20) & 1)
    setFeature(FEATURE_SSE4_2);
  if ((ECX >> 22) & 1)
    setFeature(FEATURE_MOVBE);
  if ((ECX >> 23) & 1)
    setFeature(FEATURE_POPCNT);
  if ((ECX >> 25) & 1)
    setFeature(FEATURE_AES);
  if ((ECX >> 29) & 1)
    setFeature(FEATURE_F16C);
  if ((ECX >> 30) & 1)
    setFeature(FEATURE_RDRND);

  // If CPUID indicates support for XSAVE, XRESTORE and AVX, and XGETBV
  // indicates that the AVX registers will be saved and restored on context
  // switch, then we have full AVX support.
  const unsigned AVXBits = (1 << 27) | (1 << 28);
  bool HasAVXSave = ((ECX & AVXBits) == AVXBits) && !getX86XCR0(&EAX, &EDX) &&
                    ((EAX & 0x6) == 0x6);
#if defined(__APPLE__)
  // Darwin lazily saves the AVX512 context on first use: trust that the OS will
  // save the AVX512 context if we use AVX512 instructions, even the bit is not
  // set right now.
  bool HasAVX512Save = true;
#else
  // AVX512 requires additional context to be saved by the OS.
  bool HasAVX512Save = HasAVXSave && ((EAX & 0xe0) == 0xe0);
#endif
  // AMX requires additional context to be saved by the OS.
  const unsigned AMXBits = (1 << 17) | (1 << 18);
  bool HasXSave = ((ECX >> 27) & 1) && !getX86XCR0(&EAX, &EDX);
  bool HasAMXSave = HasXSave && ((EAX & AMXBits) == AMXBits);

  // GHC deviation from upstream: FMA instructions are VEX-encoded and
  // unusable unless the OS saves the AVX register state, so FEATURE_FMA (set
  // from the raw CPUID bit above) is cleared again when full AVX support is
  // missing. This matches llvm/lib/TargetParser/Host.cpp ("fma") and GCC's
  // gcc/common/config/i386/cpuinfo.h (FEATURE_FMA under avx_usable).
  if (!HasAVXSave)
    Features[FEATURE_FMA / 32] &= ~(1U << (FEATURE_FMA % 32));

  if (HasAVXSave)
    setFeature(FEATURE_AVX);

  if (((ECX >> 26) & 1) && HasAVXSave)
    setFeature(FEATURE_XSAVE);

  bool HasLeaf7 =
      MaxLeaf >= 0x7 && !getX86CpuIDAndInfoEx(0x7, 0x0, &EAX, &EBX, &ECX, &EDX);

  if (HasLeaf7 && ((EBX >> 0) & 1))
    setFeature(FEATURE_FSGSBASE);
  if (HasLeaf7 && ((EBX >> 2) & 1))
    setFeature(FEATURE_SGX);
  if (HasLeaf7 && ((EBX >> 3) & 1))
    setFeature(FEATURE_BMI);
  if (HasLeaf7 && ((EBX >> 5) & 1) && HasAVXSave)
    setFeature(FEATURE_AVX2);
  if (HasLeaf7 && ((EBX >> 8) & 1))
    setFeature(FEATURE_BMI2);
  if (HasLeaf7 && ((EBX >> 11) & 1))
    setFeature(FEATURE_RTM);
  if (HasLeaf7 && ((EBX >> 16) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512F);
  if (HasLeaf7 && ((EBX >> 17) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512DQ);
  if (HasLeaf7 && ((EBX >> 18) & 1))
    setFeature(FEATURE_RDSEED);
  if (HasLeaf7 && ((EBX >> 19) & 1))
    setFeature(FEATURE_ADX);
  if (HasLeaf7 && ((EBX >> 21) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512IFMA);
  if (HasLeaf7 && ((EBX >> 24) & 1))
    setFeature(FEATURE_CLWB);
  if (HasLeaf7 && ((EBX >> 26) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512PF);
  if (HasLeaf7 && ((EBX >> 27) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512ER);
  if (HasLeaf7 && ((EBX >> 28) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512CD);
  if (HasLeaf7 && ((EBX >> 29) & 1))
    setFeature(FEATURE_SHA);
  if (HasLeaf7 && ((EBX >> 30) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512BW);
  if (HasLeaf7 && ((EBX >> 31) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512VL);

  if (HasLeaf7 && ((ECX >> 0) & 1))
    setFeature(FEATURE_PREFETCHWT1);
  if (HasLeaf7 && ((ECX >> 1) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512VBMI);
  if (HasLeaf7 && ((ECX >> 4) & 1))
    setFeature(FEATURE_PKU);
  if (HasLeaf7 && ((ECX >> 5) & 1))
    setFeature(FEATURE_WAITPKG);
  if (HasLeaf7 && ((ECX >> 6) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512VBMI2);
  if (HasLeaf7 && ((ECX >> 7) & 1))
    setFeature(FEATURE_SHSTK);
  if (HasLeaf7 && ((ECX >> 8) & 1))
    setFeature(FEATURE_GFNI);
  if (HasLeaf7 && ((ECX >> 9) & 1) && HasAVXSave)
    setFeature(FEATURE_VAES);
  if (HasLeaf7 && ((ECX >> 10) & 1) && HasAVXSave)
    setFeature(FEATURE_VPCLMULQDQ);
  if (HasLeaf7 && ((ECX >> 11) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512VNNI);
  if (HasLeaf7 && ((ECX >> 12) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512BITALG);
  if (HasLeaf7 && ((ECX >> 14) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512VPOPCNTDQ);
  if (HasLeaf7 && ((ECX >> 22) & 1))
    setFeature(FEATURE_RDPID);
  if (HasLeaf7 && ((ECX >> 23) & 1))
    setFeature(FEATURE_KL);
  if (HasLeaf7 && ((ECX >> 25) & 1))
    setFeature(FEATURE_CLDEMOTE);
  if (HasLeaf7 && ((ECX >> 27) & 1))
    setFeature(FEATURE_MOVDIRI);
  if (HasLeaf7 && ((ECX >> 28) & 1))
    setFeature(FEATURE_MOVDIR64B);
  if (HasLeaf7 && ((ECX >> 29) & 1))
    setFeature(FEATURE_ENQCMD);

  if (HasLeaf7 && ((EDX >> 2) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX5124VNNIW);
  if (HasLeaf7 && ((EDX >> 3) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX5124FMAPS);
  if (HasLeaf7 && ((EDX >> 5) & 1))
    setFeature(FEATURE_UINTR);
  if (HasLeaf7 && ((EDX >> 8) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512VP2INTERSECT);
  if (HasLeaf7 && ((EDX >> 14) & 1))
    setFeature(FEATURE_SERIALIZE);
  if (HasLeaf7 && ((EDX >> 16) & 1))
    setFeature(FEATURE_TSXLDTRK);
  if (HasLeaf7 && ((EDX >> 18) & 1))
    setFeature(FEATURE_PCONFIG);
  if (HasLeaf7 && ((EDX >> 22) & 1) && HasAMXSave)
    setFeature(FEATURE_AMX_BF16);
  if (HasLeaf7 && ((EDX >> 23) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512FP16);
  if (HasLeaf7 && ((EDX >> 24) & 1) && HasAMXSave)
    setFeature(FEATURE_AMX_TILE);
  if (HasLeaf7 && ((EDX >> 25) & 1) && HasAMXSave)
    setFeature(FEATURE_AMX_INT8);

  // EAX from subleaf 0 is the maximum subleaf supported. Some CPUs don't
  // return all 0s for invalid subleaves so check the limit.
  bool HasLeaf7Subleaf1 =
      HasLeaf7 && EAX >= 1 &&
      !getX86CpuIDAndInfoEx(0x7, 0x1, &EAX, &EBX, &ECX, &EDX);
  if (HasLeaf7Subleaf1 && ((EAX >> 0) & 1))
    setFeature(FEATURE_SHA512);
  if (HasLeaf7Subleaf1 && ((EAX >> 1) & 1))
    setFeature(FEATURE_SM3);
  if (HasLeaf7Subleaf1 && ((EAX >> 2) & 1))
    setFeature(FEATURE_SM4);
  if (HasLeaf7Subleaf1 && ((EAX >> 3) & 1))
    setFeature(FEATURE_RAOINT);
  if (HasLeaf7Subleaf1 && ((EAX >> 4) & 1) && HasAVXSave)
    setFeature(FEATURE_AVXVNNI);
  if (HasLeaf7Subleaf1 && ((EAX >> 5) & 1) && HasAVX512Save)
    setFeature(FEATURE_AVX512BF16);
  if (HasLeaf7Subleaf1 && ((EAX >> 7) & 1))
    setFeature(FEATURE_CMPCCXADD);
  if (HasLeaf7Subleaf1 && ((EAX >> 21) & 1) && HasAMXSave)
    setFeature(FEATURE_AMX_FP16);
  if (HasLeaf7Subleaf1 && ((EAX >> 22) & 1))
    setFeature(FEATURE_HRESET);
  if (HasLeaf7Subleaf1 && ((EAX >> 23) & 1) && HasAVXSave)
    setFeature(FEATURE_AVXIFMA);
  if (HasLeaf7Subleaf1 && ((EAX >> 31) & 1))
    setFeature(FEATURE_MOVRS);

  if (HasLeaf7Subleaf1 && ((EDX >> 4) & 1) && HasAVXSave)
    setFeature(FEATURE_AVXVNNIINT8);
  if (HasLeaf7Subleaf1 && ((EDX >> 5) & 1) && HasAVXSave)
    setFeature(FEATURE_AVXNECONVERT);
  if (HasLeaf7Subleaf1 && ((EDX >> 8) & 1) && HasAMXSave)
    setFeature(FEATURE_AMX_COMPLEX);
  if (HasLeaf7Subleaf1 && ((EDX >> 10) & 1) && HasAVXSave)
    setFeature(FEATURE_AVXVNNIINT16);
  if (HasLeaf7Subleaf1 && ((EDX >> 14) & 1))
    setFeature(FEATURE_PREFETCHI);
  if (HasLeaf7Subleaf1 && ((EDX >> 15) & 1))
    setFeature(FEATURE_USERMSR);
  if (HasLeaf7Subleaf1 && ((EDX >> 21) & 1))
    setFeature(FEATURE_APXF);

  unsigned MaxLevel = 0;
  getX86CpuIDAndInfo(0, &MaxLevel, &EBX, &ECX, &EDX);
  bool HasLeafD = MaxLevel >= 0xd &&
                  !getX86CpuIDAndInfoEx(0xd, 0x1, &EAX, &EBX, &ECX, &EDX);
  if (HasLeafD && ((EAX >> 0) & 1) && HasAVXSave)
    setFeature(FEATURE_XSAVEOPT);
  if (HasLeafD && ((EAX >> 1) & 1) && HasAVXSave)
    setFeature(FEATURE_XSAVEC);
  if (HasLeafD && ((EAX >> 3) & 1) && HasAVXSave)
    setFeature(FEATURE_XSAVES);

  bool HasLeaf24 =
      MaxLevel >= 0x24 && !getX86CpuIDAndInfo(0x24, &EAX, &EBX, &ECX, &EDX);
  if (HasLeaf7Subleaf1 && ((EDX >> 19) & 1) && HasLeaf24) {
    bool Has512Len = (EBX >> 18) & 1;
    int AVX10Ver = EBX & 0xff;
    if (AVX10Ver >= 2) {
      setFeature(FEATURE_AVX10_2_256);
      if (Has512Len)
        setFeature(FEATURE_AVX10_2_512);
    }
    if (AVX10Ver >= 1) {
      setFeature(FEATURE_AVX10_1_256);
      if (Has512Len)
        setFeature(FEATURE_AVX10_1_512);
    }
  }

  unsigned MaxExtLevel = 0;
  getX86CpuIDAndInfo(0x80000000, &MaxExtLevel, &EBX, &ECX, &EDX);

  bool HasExtLeaf1 = MaxExtLevel >= 0x80000001 &&
                     !getX86CpuIDAndInfo(0x80000001, &EAX, &EBX, &ECX, &EDX);
  if (HasExtLeaf1) {
    if (ECX & 1)
      setFeature(FEATURE_LAHF_LM);
    if ((ECX >> 5) & 1)
      setFeature(FEATURE_LZCNT);
    if (((ECX >> 6) & 1))
      setFeature(FEATURE_SSE4_A);
    if (((ECX >> 8) & 1))
      setFeature(FEATURE_PRFCHW);
    if (((ECX >> 11) & 1))
      setFeature(FEATURE_XOP);
    if (((ECX >> 15) & 1))
      setFeature(FEATURE_LWP);
    if (((ECX >> 16) & 1))
      setFeature(FEATURE_FMA4);
    if (((ECX >> 21) & 1))
      setFeature(FEATURE_TBM);
    if (((ECX >> 29) & 1))
      setFeature(FEATURE_MWAITX);

    if (((EDX >> 29) & 1))
      setFeature(FEATURE_LM);
  }

  bool HasExtLeaf8 = MaxExtLevel >= 0x80000008 &&
                     !getX86CpuIDAndInfo(0x80000008, &EAX, &EBX, &ECX, &EDX);
  if (HasExtLeaf8 && ((EBX >> 0) & 1))
    setFeature(FEATURE_CLZERO);
  if (HasExtLeaf8 && ((EBX >> 9) & 1))
    setFeature(FEATURE_WBNOINVD);

  bool HasLeaf14 = MaxLevel >= 0x14 &&
                   !getX86CpuIDAndInfoEx(0x14, 0x0, &EAX, &EBX, &ECX, &EDX);
  if (HasLeaf14 && ((EBX >> 4) & 1))
    setFeature(FEATURE_PTWRITE);

  bool HasLeaf19 =
      MaxLevel >= 0x19 && !getX86CpuIDAndInfo(0x19, &EAX, &EBX, &ECX, &EDX);
  if (HasLeaf7 && HasLeaf19 && ((EBX >> 2) & 1))
    setFeature(FEATURE_WIDEKL);

  if (hasFeature(FEATURE_LM) && hasFeature(FEATURE_SSE2)) {
    setFeature(FEATURE_X86_64_BASELINE);
    if (hasFeature(FEATURE_CMPXCHG16B) && hasFeature(FEATURE_POPCNT) &&
        hasFeature(FEATURE_LAHF_LM) && hasFeature(FEATURE_SSE4_2)) {
      setFeature(FEATURE_X86_64_V2);
      if (hasFeature(FEATURE_AVX2) && hasFeature(FEATURE_BMI) &&
          hasFeature(FEATURE_BMI2) && hasFeature(FEATURE_F16C) &&
          hasFeature(FEATURE_FMA) && hasFeature(FEATURE_LZCNT) &&
          hasFeature(FEATURE_MOVBE)) {
        setFeature(FEATURE_X86_64_V3);
        if (hasFeature(FEATURE_AVX512BW) && hasFeature(FEATURE_AVX512CD) &&
            hasFeature(FEATURE_AVX512DQ) && hasFeature(FEATURE_AVX512VL))
          setFeature(FEATURE_X86_64_V4);
      }
    }
  }

#undef hasFeature
#undef setFeature
}

#endif /* GHC_HOST_IS_X86 */

/* The driver, modeled on upstream's __cpu_indicator_init: same CPUID call
 * sequence, but the first 64 feature bits are returned to the caller (see
 * GHC.Driver.CpuFeatures) instead of being stored in the __cpu_model
 * globals. */
HsWord64 ghc_detect_x86_cpu_features(void)
{
#if defined(GHC_HOST_IS_X86)
  unsigned EAX = 0, EBX = 0, ECX = 0, EDX = 0;
  unsigned MaxLeaf = 5;
  unsigned Features[(CPU_FEATURE_MAX + 31) / 32] = {0};

  if (getX86CpuIDAndInfo(0, &MaxLeaf, &EBX, &ECX, &EDX) || MaxLeaf < 1)
    return 0;

  getX86CpuIDAndInfo(1, &EAX, &EBX, &ECX, &EDX);

  // Find available features.
  getAvailableFeatures(ECX, EDX, MaxLeaf, &Features[0]);

  return ((HsWord64)Features[1] << 32) | (HsWord64)Features[0];
#else
  return 0;
#endif
}

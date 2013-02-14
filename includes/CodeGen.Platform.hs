
import CmmExpr
import FastBool
#if !(MACHREGS_i386 || MACHREGS_x86_64 || MACHREGS_sparc || MACHREGS_powerpc)
import Panic
#endif
import Reg

#include "ghcautoconf.h"
#include "stg/MachRegs.h"

#if MACHREGS_i386 || MACHREGS_x86_64

# if MACHREGS_i386
#  define eax 0
#  define ebx 1
#  define ecx 2
#  define edx 3
#  define esi 4
#  define edi 5
#  define ebp 6
#  define esp 7
# endif

# if MACHREGS_x86_64
#  define rax   0
#  define rbx   1
#  define rcx   2
#  define rdx   3
#  define rsi   4
#  define rdi   5
#  define rbp   6
#  define rsp   7
#  define r8    8
#  define r9    9
#  define r10   10
#  define r11   11
#  define r12   12
#  define r13   13
#  define r14   14
#  define r15   15
# endif

# define fake0 16
# define fake1 17
# define fake2 18
# define fake3 19
# define fake4 20
# define fake5 21

# define xmm0  24
# define xmm1  25
# define xmm2  26
# define xmm3  27
# define xmm4  28
# define xmm5  29
# define xmm6  30
# define xmm7  31
# define xmm8  32
# define xmm9  33
# define xmm10 34
# define xmm11 35
# define xmm12 36
# define xmm13 37
# define xmm14 38
# define xmm15 39

# define ymm0  40
# define ymm1  41
# define ymm2  42
# define ymm3  43
# define ymm4  44
# define ymm5  45
# define ymm6  46
# define ymm7  47
# define ymm8  48
# define ymm9  49
# define ymm10 50
# define ymm11 51
# define ymm12 52
# define ymm13 53
# define ymm14 54
# define ymm15 55

#elif MACHREGS_powerpc

# define r0 0
# define r1 1
# define r2 2
# define r3 3
# define r4 4
# define r5 5
# define r6 6
# define r7 7
# define r8 8
# define r9 9
# define r10 10
# define r11 11
# define r12 12
# define r13 13
# define r14 14
# define r15 15
# define r16 16
# define r17 17
# define r18 18
# define r19 19
# define r20 20
# define r21 21
# define r22 22
# define r23 23
# define r24 24
# define r25 25
# define r26 26
# define r27 27
# define r28 28
# define r29 29
# define r30 30
# define r31 31

# if MACHREGS_darwin
#  define f0  32
#  define f1  33
#  define f2  34
#  define f3  35
#  define f4  36
#  define f5  37
#  define f6  38
#  define f7  39
#  define f8  40
#  define f9  41
#  define f10 42
#  define f11 43
#  define f12 44
#  define f13 45
#  define f14 46
#  define f15 47
#  define f16 48
#  define f17 49
#  define f18 50
#  define f19 51
#  define f20 52
#  define f21 53
#  define f22 54
#  define f23 55
#  define f24 56
#  define f25 57
#  define f26 58
#  define f27 59
#  define f28 60
#  define f29 61
#  define f30 62
#  define f31 63
# else
#  define fr0  32
#  define fr1  33
#  define fr2  34
#  define fr3  35
#  define fr4  36
#  define fr5  37
#  define fr6  38
#  define fr7  39
#  define fr8  40
#  define fr9  41
#  define fr10 42
#  define fr11 43
#  define fr12 44
#  define fr13 45
#  define fr14 46
#  define fr15 47
#  define fr16 48
#  define fr17 49
#  define fr18 50
#  define fr19 51
#  define fr20 52
#  define fr21 53
#  define fr22 54
#  define fr23 55
#  define fr24 56
#  define fr25 57
#  define fr26 58
#  define fr27 59
#  define fr28 60
#  define fr29 61
#  define fr30 62
#  define fr31 63
# endif

#elif MACHREGS_sparc

# define g0  0
# define g1  1
# define g2  2
# define g3  3
# define g4  4
# define g5  5
# define g6  6
# define g7  7

# define o0  8
# define o1  9
# define o2  10
# define o3  11
# define o4  12
# define o5  13
# define o6  14
# define o7  15

# define l0  16
# define l1  17
# define l2  18
# define l3  19
# define l4  20
# define l5  21
# define l6  22
# define l7  23

# define i0  24
# define i1  25
# define i2  26
# define i3  27
# define i4  28
# define i5  29
# define i6  30
# define i7  31

# define f0  32
# define f1  33
# define f2  34
# define f3  35
# define f4  36
# define f5  37
# define f6  38
# define f7  39
# define f8  40
# define f9  41
# define f10 42
# define f11 43
# define f12 44
# define f13 45
# define f14 46
# define f15 47
# define f16 48
# define f17 49
# define f18 50
# define f19 51
# define f20 52
# define f21 53
# define f22 54
# define f23 55
# define f24 56
# define f25 57
# define f26 58
# define f27 59
# define f28 60
# define f29 61
# define f30 62
# define f31 63

#endif

callerSaves :: GlobalReg -> Bool
#ifdef CALLER_SAVES_Base
callerSaves BaseReg           = True
#endif
#ifdef CALLER_SAVES_R1
callerSaves (VanillaReg 1 _)  = True
#endif
#ifdef CALLER_SAVES_R2
callerSaves (VanillaReg 2 _)  = True
#endif
#ifdef CALLER_SAVES_R3
callerSaves (VanillaReg 3 _)  = True
#endif
#ifdef CALLER_SAVES_R4
callerSaves (VanillaReg 4 _)  = True
#endif
#ifdef CALLER_SAVES_R5
callerSaves (VanillaReg 5 _)  = True
#endif
#ifdef CALLER_SAVES_R6
callerSaves (VanillaReg 6 _)  = True
#endif
#ifdef CALLER_SAVES_R7
callerSaves (VanillaReg 7 _)  = True
#endif
#ifdef CALLER_SAVES_R8
callerSaves (VanillaReg 8 _)  = True
#endif
#ifdef CALLER_SAVES_R9
callerSaves (VanillaReg 9 _)  = True
#endif
#ifdef CALLER_SAVES_R10
callerSaves (VanillaReg 10 _) = True
#endif
#ifdef CALLER_SAVES_F1
callerSaves (FloatReg 1)      = True
#endif
#ifdef CALLER_SAVES_F2
callerSaves (FloatReg 2)      = True
#endif
#ifdef CALLER_SAVES_F3
callerSaves (FloatReg 3)      = True
#endif
#ifdef CALLER_SAVES_F4
callerSaves (FloatReg 4)      = True
#endif
#ifdef CALLER_SAVES_F5
callerSaves (FloatReg 5)      = True
#endif
#ifdef CALLER_SAVES_F6
callerSaves (FloatReg 6)      = True
#endif
#ifdef CALLER_SAVES_D1
callerSaves (DoubleReg 1)     = True
#endif
#ifdef CALLER_SAVES_D2
callerSaves (DoubleReg 2)     = True
#endif
#ifdef CALLER_SAVES_D3
callerSaves (DoubleReg 3)     = True
#endif
#ifdef CALLER_SAVES_D4
callerSaves (DoubleReg 4)     = True
#endif
#ifdef CALLER_SAVES_D5
callerSaves (DoubleReg 5)     = True
#endif
#ifdef CALLER_SAVES_D6
callerSaves (DoubleReg 6)     = True
#endif
#ifdef CALLER_SAVES_L1
callerSaves (LongReg 1)       = True
#endif
#ifdef CALLER_SAVES_Sp
callerSaves Sp                = True
#endif
#ifdef CALLER_SAVES_SpLim
callerSaves SpLim             = True
#endif
#ifdef CALLER_SAVES_Hp
callerSaves Hp                = True
#endif
#ifdef CALLER_SAVES_HpLim
callerSaves HpLim             = True
#endif
#ifdef CALLER_SAVES_CCCS
callerSaves CCCS              = True
#endif
#ifdef CALLER_SAVES_CurrentTSO
callerSaves CurrentTSO        = True
#endif
#ifdef CALLER_SAVES_CurrentNursery
callerSaves CurrentNursery    = True
#endif
callerSaves _                 = False

activeStgRegs :: [GlobalReg]
activeStgRegs = [
#ifdef REG_Base
    BaseReg
#endif
#ifdef REG_Sp
    ,Sp
#endif
#ifdef REG_Hp
    ,Hp
#endif
#ifdef REG_R1
    ,VanillaReg 1 VGcPtr
#endif
#ifdef REG_R2
    ,VanillaReg 2 VGcPtr
#endif
#ifdef REG_R3
    ,VanillaReg 3 VGcPtr
#endif
#ifdef REG_R4
    ,VanillaReg 4 VGcPtr
#endif
#ifdef REG_R5
    ,VanillaReg 5 VGcPtr
#endif
#ifdef REG_R6
    ,VanillaReg 6 VGcPtr
#endif
#ifdef REG_R7
    ,VanillaReg 7 VGcPtr
#endif
#ifdef REG_R8
    ,VanillaReg 8 VGcPtr
#endif
#ifdef REG_R9
    ,VanillaReg 9 VGcPtr
#endif
#ifdef REG_R10
    ,VanillaReg 10 VGcPtr
#endif
#ifdef REG_SpLim
    ,SpLim
#endif
#if MAX_REAL_XMM_REG != 0
#ifdef REG_F1
    ,FloatReg 1
#endif
#ifdef REG_D1
    ,DoubleReg 1
#endif
#ifdef REG_XMM1
    ,XmmReg 1
#endif
#ifdef REG_YMM1
    ,YmmReg 1
#endif
#ifdef REG_F2
    ,FloatReg 2
#endif
#ifdef REG_D2
    ,DoubleReg 2
#endif
#ifdef REG_XMM2
    ,XmmReg 2
#endif
#ifdef REG_YMM2
    ,YmmReg 2
#endif
#ifdef REG_F3
    ,FloatReg 3
#endif
#ifdef REG_D3
    ,DoubleReg 3
#endif
#ifdef REG_XMM3
    ,XmmReg 3
#endif
#ifdef REG_YMM3
    ,YmmReg 3
#endif
#ifdef REG_F4
    ,FloatReg 4
#endif
#ifdef REG_D4
    ,DoubleReg 4
#endif
#ifdef REG_XMM4
    ,XmmReg 4
#endif
#ifdef REG_YMM4
    ,YmmReg 4
#endif
#ifdef REG_F5
    ,FloatReg 5
#endif
#ifdef REG_D5
    ,DoubleReg 5
#endif
#ifdef REG_XMM5
    ,XmmReg 5
#endif
#ifdef REG_YMM5
    ,YmmReg 5
#endif
#ifdef REG_F6
    ,FloatReg 6
#endif
#ifdef REG_D6
    ,DoubleReg 6
#endif
#ifdef REG_XMM6
    ,XmmReg 6
#endif
#ifdef REG_YMM6
    ,YmmReg 6
#endif
#else /* MAX_REAL_XMM_REG == 0 */
#ifdef REG_F1
    ,FloatReg 1
#endif
#ifdef REG_F2
    ,FloatReg 2
#endif
#ifdef REG_F3
    ,FloatReg 3
#endif
#ifdef REG_F4
    ,FloatReg 4
#endif
#ifdef REG_F5
    ,FloatReg 5
#endif
#ifdef REG_F6
    ,FloatReg 6
#endif
#ifdef REG_D1
    ,DoubleReg 1
#endif
#ifdef REG_D2
    ,DoubleReg 2
#endif
#ifdef REG_D3
    ,DoubleReg 3
#endif
#ifdef REG_D4
    ,DoubleReg 4
#endif
#ifdef REG_D5
    ,DoubleReg 5
#endif
#ifdef REG_D6
    ,DoubleReg 6
#endif
#endif /* MAX_REAL_XMM_REG == 0 */
    ]

haveRegBase :: Bool
#ifdef REG_Base
haveRegBase = True
#else
haveRegBase = False
#endif

--  | Returns 'Nothing' if this global register is not stored
-- in a real machine register, otherwise returns @'Just' reg@, where
-- reg is the machine register it is stored in.
globalRegMaybe :: GlobalReg -> Maybe RealReg
#if MACHREGS_i386 || MACHREGS_x86_64 || MACHREGS_sparc || MACHREGS_powerpc
# ifdef REG_Base
globalRegMaybe BaseReg                  = Just (RealRegSingle REG_Base)
# endif
# ifdef REG_R1
globalRegMaybe (VanillaReg 1 _)         = Just (RealRegSingle REG_R1)
# endif
# ifdef REG_R2
globalRegMaybe (VanillaReg 2 _)         = Just (RealRegSingle REG_R2)
# endif
# ifdef REG_R3
globalRegMaybe (VanillaReg 3 _)         = Just (RealRegSingle REG_R3)
# endif
# ifdef REG_R4
globalRegMaybe (VanillaReg 4 _)         = Just (RealRegSingle REG_R4)
# endif
# ifdef REG_R5
globalRegMaybe (VanillaReg 5 _)         = Just (RealRegSingle REG_R5)
# endif
# ifdef REG_R6
globalRegMaybe (VanillaReg 6 _)         = Just (RealRegSingle REG_R6)
# endif
# ifdef REG_R7
globalRegMaybe (VanillaReg 7 _)         = Just (RealRegSingle REG_R7)
# endif
# ifdef REG_R8
globalRegMaybe (VanillaReg 8 _)         = Just (RealRegSingle REG_R8)
# endif
# ifdef REG_R9
globalRegMaybe (VanillaReg 9 _)         = Just (RealRegSingle REG_R9)
# endif
# ifdef REG_R10
globalRegMaybe (VanillaReg 10 _)        = Just (RealRegSingle REG_R10)
# endif
# ifdef REG_F1
globalRegMaybe (FloatReg 1)             = Just (RealRegSingle REG_F1)
# endif
# ifdef REG_F2
globalRegMaybe (FloatReg 2)             = Just (RealRegSingle REG_F2)
# endif
# ifdef REG_F3
globalRegMaybe (FloatReg 3)             = Just (RealRegSingle REG_F3)
# endif
# ifdef REG_F4
globalRegMaybe (FloatReg 4)             = Just (RealRegSingle REG_F4)
# endif
# ifdef REG_F5
globalRegMaybe (FloatReg 5)             = Just (RealRegSingle REG_F5)
# endif
# ifdef REG_F6
globalRegMaybe (FloatReg 6)             = Just (RealRegSingle REG_F6)
# endif
# ifdef REG_D1
globalRegMaybe (DoubleReg 1)            =
#  if MACHREGS_sparc
                                          Just (RealRegPair REG_D1 (REG_D1 + 1))
#  else
                                          Just (RealRegSingle REG_D1)
#  endif
# endif
# ifdef REG_D2
globalRegMaybe (DoubleReg 2)            =
#  if MACHREGS_sparc
                                          Just (RealRegPair REG_D2 (REG_D2 + 1))
#  else
                                          Just (RealRegSingle REG_D2)
#  endif
# endif
# ifdef REG_D3
globalRegMaybe (DoubleReg 3)            =
#  if MACHREGS_sparc
                                          Just (RealRegPair REG_D3 (REG_D3 + 1))
#  else
                                          Just (RealRegSingle REG_D3)
#  endif
# endif
# ifdef REG_D4
globalRegMaybe (DoubleReg 4)            =
#  if MACHREGS_sparc
                                          Just (RealRegPair REG_D4 (REG_D4 + 1))
#  else
                                          Just (RealRegSingle REG_D4)
#  endif
# endif
# ifdef REG_D5
globalRegMaybe (DoubleReg 5)            =
#  if MACHREGS_sparc
                                          Just (RealRegPair REG_D5 (REG_D5 + 1))
#  else
                                          Just (RealRegSingle REG_D5)
#  endif
# endif
# ifdef REG_D6
globalRegMaybe (DoubleReg 6)            =
#  if MACHREGS_sparc
                                          Just (RealRegPair REG_D6 (REG_D6 + 1))
#  else
                                          Just (RealRegSingle REG_D6)
#  endif
# endif
# if MAX_REAL_XMM_REG != 0
#  ifdef REG_XMM1
globalRegMaybe (XmmReg 1)               = Just (RealRegSingle REG_XMM1)
#  endif
#  ifdef REG_XMM2
globalRegMaybe (XmmReg 2)               = Just (RealRegSingle REG_XMM2)
#  endif
#  ifdef REG_XMM3
globalRegMaybe (XmmReg 3)               = Just (RealRegSingle REG_XMM3)
#  endif
#  ifdef REG_XMM4
globalRegMaybe (XmmReg 4)               = Just (RealRegSingle REG_XMM4)
#  endif
#  ifdef REG_XMM5
globalRegMaybe (XmmReg 5)               = Just (RealRegSingle REG_XMM5)
#  endif
#  ifdef REG_XMM6
globalRegMaybe (XmmReg 6)               = Just (RealRegSingle REG_XMM6)
#  endif
# endif
# if MAX_REAL_YMM_REG != 0
#  ifdef REG_YMM1
globalRegMaybe (YmmReg 1)               = Just (RealRegSingle REG_YMM1)
#  endif
#  ifdef REG_YMM2
globalRegMaybe (YmmReg 2)               = Just (RealRegSingle REG_YMM2)
#  endif
#  ifdef REG_YMM3
globalRegMaybe (YmmReg 3)               = Just (RealRegSingle REG_YMM3)
#  endif
#  ifdef REG_YMM4
globalRegMaybe (YmmReg 4)               = Just (RealRegSingle REG_YMM4)
#  endif
#  ifdef REG_YMM5
globalRegMaybe (YmmReg 5)               = Just (RealRegSingle REG_YMM5)
#  endif
#  ifdef REG_YMM6
globalRegMaybe (YmmReg 6)               = Just (RealRegSingle REG_YMM6)
#  endif
# endif
# ifdef REG_Sp
globalRegMaybe Sp                       = Just (RealRegSingle REG_Sp)
# endif
# ifdef REG_Lng1
globalRegMaybe (LongReg 1)              = Just (RealRegSingle REG_Lng1)
# endif
# ifdef REG_Lng2
globalRegMaybe (LongReg 2)              = Just (RealRegSingle REG_Lng2)
# endif
# ifdef REG_SpLim
globalRegMaybe SpLim                    = Just (RealRegSingle REG_SpLim)
# endif
# ifdef REG_Hp
globalRegMaybe Hp                       = Just (RealRegSingle REG_Hp)
# endif
# ifdef REG_HpLim
globalRegMaybe HpLim                    = Just (RealRegSingle REG_HpLim)
# endif
# ifdef REG_CurrentTSO
globalRegMaybe CurrentTSO               = Just (RealRegSingle REG_CurrentTSO)
# endif
# ifdef REG_CurrentNursery
globalRegMaybe CurrentNursery           = Just (RealRegSingle REG_CurrentNursery)
# endif
globalRegMaybe _                        = Nothing
#else
globalRegMaybe = panic "globalRegMaybe not defined for this platform"
#endif

freeReg :: RegNo -> FastBool

#if MACHREGS_i386 || MACHREGS_x86_64

# if MACHREGS_i386
freeReg esp = fastBool False -- %esp is the C stack pointer
freeReg esi = fastBool False -- Note [esi/edi not allocatable]
freeReg edi = fastBool False
# endif
# if MACHREGS_x86_64
freeReg rsp = fastBool False  --        %rsp is the C stack pointer
# endif

{-
Note [esi/edi not allocatable]

%esi is mapped to R1, so %esi would normally be allocatable while it
is not being used for R1.  However, %esi has no 8-bit version on x86,
and the linear register allocator is not sophisticated enough to
handle this irregularity (we need more RegClasses).  The
graph-colouring allocator also cannot handle this - it was designed
with more flexibility in mind, but the current implementation is
restricted to the same set of classes as the linear allocator.

Hence, on x86 esi and edi are treated as not allocatable.
-}

-- split patterns in two functions to prevent overlaps
freeReg r         = freeRegBase r

freeRegBase :: RegNo -> FastBool
# ifdef REG_Base
freeRegBase REG_Base = fastBool False
# endif
# ifdef REG_Sp
freeRegBase REG_Sp   = fastBool False
# endif
# ifdef REG_SpLim
freeRegBase REG_SpLim = fastBool False
# endif
# ifdef REG_Hp
freeRegBase REG_Hp   = fastBool False
# endif
# ifdef REG_HpLim
freeRegBase REG_HpLim = fastBool False
# endif
-- All other regs are considered to be "free", because we can track
-- their liveness accurately.
freeRegBase _ = fastBool True

#elif MACHREGS_powerpc

freeReg 0 = fastBool False -- Hack: r0 can't be used in all insns,
                           -- but it's actually free
freeReg 1 = fastBool False -- The Stack Pointer
# if !MACHREGS_darwin
-- most non-darwin powerpc OSes use r2 as a TOC pointer or something like that
freeReg 2 = fastBool False
# endif
# ifdef REG_Base
freeReg REG_Base = fastBool False
# endif
# ifdef REG_R1
freeReg REG_R1   = fastBool False
# endif
# ifdef REG_R2
freeReg REG_R2   = fastBool False
# endif
# ifdef REG_R3
freeReg REG_R3   = fastBool False
# endif
# ifdef REG_R4
freeReg REG_R4   = fastBool False
# endif
# ifdef REG_R5
freeReg REG_R5   = fastBool False
# endif
# ifdef REG_R6
freeReg REG_R6   = fastBool False
# endif
# ifdef REG_R7
freeReg REG_R7   = fastBool False
# endif
# ifdef REG_R8
freeReg REG_R8   = fastBool False
# endif
# ifdef REG_R9
freeReg REG_R9   = fastBool False
# endif
# ifdef REG_R10
freeReg REG_R10  = fastBool False
# endif
# ifdef REG_F1
freeReg REG_F1 = fastBool False
# endif
# ifdef REG_F2
freeReg REG_F2 = fastBool False
# endif
# ifdef REG_F3
freeReg REG_F3 = fastBool False
# endif
# ifdef REG_F4
freeReg REG_F4 = fastBool False
# endif
# ifdef REG_F5
freeReg REG_F5 = fastBool False
# endif
# ifdef REG_F6
freeReg REG_F6 = fastBool False
# endif
# ifdef REG_D1
freeReg REG_D1 = fastBool False
# endif
# ifdef REG_D2
freeReg REG_D2 = fastBool False
# endif
# ifdef REG_D3
freeReg REG_D3 = fastBool False
# endif
# ifdef REG_D4
freeReg REG_D4 = fastBool False
# endif
# ifdef REG_D5
freeReg REG_D5 = fastBool False
# endif
# ifdef REG_D6
freeReg REG_D6 = fastBool False
# endif
# ifdef REG_Sp
freeReg REG_Sp   = fastBool False
# endif
# ifdef REG_Su
freeReg REG_Su   = fastBool False
# endif
# ifdef REG_SpLim
freeReg REG_SpLim = fastBool False
# endif
# ifdef REG_Hp
freeReg REG_Hp   = fastBool False
# endif
# ifdef REG_HpLim
freeReg REG_HpLim = fastBool False
# endif
freeReg _               = fastBool True

#elif MACHREGS_sparc

-- SPARC regs used by the OS / ABI
-- %g0(r0) is always zero
freeReg g0  = fastBool False

-- %g5(r5) - %g7(r7)
--  are reserved for the OS
freeReg g5  = fastBool False
freeReg g6  = fastBool False
freeReg g7  = fastBool False

-- %o6(r14)
--  is the C stack pointer
freeReg o6  = fastBool False

-- %o7(r15)
--  holds the C return address
freeReg o7  = fastBool False

-- %i6(r30)
--  is the C frame pointer
freeReg i6  = fastBool False

-- %i7(r31)
--  is used for C return addresses
freeReg i7  = fastBool False

-- %f0(r32) - %f1(r32)
--  are C floating point return regs
freeReg f0  = fastBool False
freeReg f1  = fastBool False

{-
freeReg regNo
    -- don't release high half of double regs
    | regNo >= f0
    , regNo <  NCG_FirstFloatReg
    , regNo `mod` 2 /= 0
    = fastBool False
-}

# ifdef REG_Base
freeReg REG_Base = fastBool False
# endif
# ifdef REG_R1
freeReg REG_R1  = fastBool False
# endif
# ifdef REG_R2
freeReg REG_R2  = fastBool False
# endif
# ifdef REG_R3
freeReg REG_R3  = fastBool False
# endif
# ifdef REG_R4
freeReg REG_R4  = fastBool False
# endif
# ifdef REG_R5
freeReg REG_R5  = fastBool False
# endif
# ifdef REG_R6
freeReg REG_R6  = fastBool False
# endif
# ifdef REG_R7
freeReg REG_R7  = fastBool False
# endif
# ifdef REG_R8
freeReg REG_R8  = fastBool False
# endif
# ifdef REG_R9
freeReg REG_R9  = fastBool False
# endif
# ifdef REG_R10
freeReg REG_R10 = fastBool False
# endif
# ifdef REG_F1
freeReg REG_F1  = fastBool False
# endif
# ifdef REG_F2
freeReg REG_F2  = fastBool False
# endif
# ifdef REG_F3
freeReg REG_F3  = fastBool False
# endif
# ifdef REG_F4
freeReg REG_F4  = fastBool False
# endif
# ifdef REG_F5
freeReg REG_F5  = fastBool False
# endif
# ifdef REG_F6
freeReg REG_F6  = fastBool False
# endif
# ifdef REG_D1
freeReg REG_D1  = fastBool False
# endif
# ifdef REG_D1_2
freeReg REG_D1_2 = fastBool False
# endif
# ifdef REG_D2
freeReg REG_D2  = fastBool False
# endif
# ifdef REG_D2_2
freeReg REG_D2_2 = fastBool False
# endif
# ifdef REG_D3
freeReg REG_D3  = fastBool False
# endif
# ifdef REG_D3_2
freeReg REG_D3_2 = fastBool False
# endif
# ifdef REG_D4
freeReg REG_D4  = fastBool False
# endif
# ifdef REG_D4_2
freeReg REG_D4_2 = fastBool False
# endif
# ifdef REG_D5
freeReg REG_D5  = fastBool False
# endif
# ifdef REG_D5_2
freeReg REG_D5_2 = fastBool False
# endif
# ifdef REG_D6
freeReg REG_D6  = fastBool False
# endif
# ifdef REG_D6_2
freeReg REG_D6_2 = fastBool False
# endif
# ifdef REG_Sp
freeReg REG_Sp  = fastBool False
# endif
# ifdef REG_Su
freeReg REG_Su  = fastBool False
# endif
# ifdef REG_SpLim
freeReg REG_SpLim = fastBool False
# endif
# ifdef REG_Hp
freeReg REG_Hp  = fastBool False
# endif
# ifdef REG_HpLim
freeReg REG_HpLim = fastBool False
# endif
freeReg _   = fastBool True

#else

freeReg = panic "freeReg not defined for this platform"

#endif


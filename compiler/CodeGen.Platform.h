
import GHC.Cmm.Expr
#if !(defined(MACHREGS_i386) || defined(MACHREGS_x86_64) \
    || defined(MACHREGS_powerpc) || defined(MACHREGS_aarch64))
import GHC.Utils.Panic.Plain
#endif
import GHC.Platform.Reg

#include "MachRegs.h"

#if defined(MACHREGS_i386) || defined(MACHREGS_x86_64)

# if defined(MACHREGS_i386)
#  define eax 0
#  define ebx 1
#  define ecx 2
#  define edx 3
#  define esi 4
#  define edi 5
#  define ebp 6
#  define esp 7
# endif

# if defined(MACHREGS_x86_64)
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


-- N.B. XMM, YMM, and ZMM are all aliased to the same hardware registers hence
-- being assigned the same RegNos.
# define xmm0  16
# define xmm1  17
# define xmm2  18
# define xmm3  19
# define xmm4  20
# define xmm5  21
# define xmm6  22
# define xmm7  23
# define xmm8  24
# define xmm9  25
# define xmm10 26
# define xmm11 27
# define xmm12 28
# define xmm13 29
# define xmm14 30
# define xmm15 31

# define ymm0  16
# define ymm1  17
# define ymm2  18
# define ymm3  19
# define ymm4  20
# define ymm5  21
# define ymm6  22
# define ymm7  23
# define ymm8  24
# define ymm9  25
# define ymm10 26
# define ymm11 27
# define ymm12 28
# define ymm13 29
# define ymm14 30
# define ymm15 31

# define zmm0  16
# define zmm1  17
# define zmm2  18
# define zmm3  19
# define zmm4  20
# define zmm5  21
# define zmm6  22
# define zmm7  23
# define zmm8  24
# define zmm9  25
# define zmm10 26
# define zmm11 27
# define zmm12 28
# define zmm13 29
# define zmm14 30
# define zmm15 31

-- Note: these are only needed for ARM/AArch64 because globalRegMaybe is now used in CmmSink.hs.
-- Since it's only used to check 'isJust', the actual values don't matter, thus
-- I'm not sure if these are the correct numberings.
-- Normally, the register names are just stringified as part of the REG() macro

#elif defined(MACHREGS_powerpc) || defined(MACHREGS_arm) \
    || defined(MACHREGS_aarch64)

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

-- See note above. These aren't actually used for anything except satisfying the compiler for globalRegMaybe
-- so I'm unsure if they're the correct numberings, should they ever be attempted to be used in the NCG.
#if defined(MACHREGS_aarch64) || defined(MACHREGS_arm)
# define s0 32
# define s1 33
# define s2 34
# define s3 35
# define s4 36
# define s5 37
# define s6 38
# define s7 39
# define s8 40
# define s9 41
# define s10 42
# define s11 43
# define s12 44
# define s13 45
# define s14 46
# define s15 47
# define s16 48
# define s17 49
# define s18 50
# define s19 51
# define s20 52
# define s21 53
# define s22 54
# define s23 55
# define s24 56
# define s25 57
# define s26 58
# define s27 59
# define s28 60
# define s29 61
# define s30 62
# define s31 63

# define d0 32
# define d1 33
# define d2 34
# define d3 35
# define d4 36
# define d5 37
# define d6 38
# define d7 39
# define d8 40
# define d9 41
# define d10 42
# define d11 43
# define d12 44
# define d13 45
# define d14 46
# define d15 47
# define d16 48
# define d17 49
# define d18 50
# define d19 51
# define d20 52
# define d21 53
# define d22 54
# define d23 55
# define d24 56
# define d25 57
# define d26 58
# define d27 59
# define d28 60
# define d29 61
# define d30 62
# define d31 63
#endif

# if defined(MACHREGS_darwin)
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

#elif defined(MACHREGS_s390x)

# define r0   0
# define r1   1
# define r2   2
# define r3   3
# define r4   4
# define r5   5
# define r6   6
# define r7   7
# define r8   8
# define r9   9
# define r10 10
# define r11 11
# define r12 12
# define r13 13
# define r14 14
# define r15 15

# define f0  16
# define f1  17
# define f2  18
# define f3  19
# define f4  20
# define f5  21
# define f6  22
# define f7  23
# define f8  24
# define f9  25
# define f10 26
# define f11 27
# define f12 28
# define f13 29
# define f14 30
# define f15 31

#elif defined(MACHREGS_riscv64)

# define zero 0
# define ra   1
# define sp   2
# define gp   3
# define tp   4
# define t0   5
# define t1   6
# define t2   7
# define s0   8
# define s1   9
# define a0  10
# define a1  11
# define a2  12
# define a3  13
# define a4  14
# define a5  15
# define a6  16
# define a7  17
# define s2  18
# define s3  19
# define s4  20
# define s5  21
# define s6  22
# define s7  23
# define s8  24
# define s9  25
# define s10 26
# define s11 27
# define t3  28
# define t4  29
# define t5  30
# define t6  31

# define ft0  32
# define ft1  33
# define ft2  34
# define ft3  35
# define ft4  36
# define ft5  37
# define ft6  38
# define ft7  39
# define fs0  40
# define fs1  41
# define fa0  42
# define fa1  43
# define fa2  44
# define fa3  45
# define fa4  46
# define fa5  47
# define fa6  48
# define fa7  49
# define fs2  50
# define fs3  51
# define fs4  52
# define fs5  53
# define fs6  54
# define fs7  55
# define fs8  56
# define fs9  57
# define fs10 58
# define fs11 59
# define ft8  60
# define ft9  61
# define ft10 62
# define ft11 63

#endif

callerSaves :: GlobalReg -> Bool
#if defined(CALLER_SAVES_Base)
callerSaves BaseReg           = True
#endif
#if defined(CALLER_SAVES_R1)
callerSaves (VanillaReg 1 _)  = True
#endif
#if defined(CALLER_SAVES_R2)
callerSaves (VanillaReg 2 _)  = True
#endif
#if defined(CALLER_SAVES_R3)
callerSaves (VanillaReg 3 _)  = True
#endif
#if defined(CALLER_SAVES_R4)
callerSaves (VanillaReg 4 _)  = True
#endif
#if defined(CALLER_SAVES_R5)
callerSaves (VanillaReg 5 _)  = True
#endif
#if defined(CALLER_SAVES_R6)
callerSaves (VanillaReg 6 _)  = True
#endif
#if defined(CALLER_SAVES_R7)
callerSaves (VanillaReg 7 _)  = True
#endif
#if defined(CALLER_SAVES_R8)
callerSaves (VanillaReg 8 _)  = True
#endif
#if defined(CALLER_SAVES_R9)
callerSaves (VanillaReg 9 _)  = True
#endif
#if defined(CALLER_SAVES_R10)
callerSaves (VanillaReg 10 _) = True
#endif
#if defined(CALLER_SAVES_F1)
callerSaves (FloatReg 1)      = True
#endif
#if defined(CALLER_SAVES_F2)
callerSaves (FloatReg 2)      = True
#endif
#if defined(CALLER_SAVES_F3)
callerSaves (FloatReg 3)      = True
#endif
#if defined(CALLER_SAVES_F4)
callerSaves (FloatReg 4)      = True
#endif
#if defined(CALLER_SAVES_F5)
callerSaves (FloatReg 5)      = True
#endif
#if defined(CALLER_SAVES_F6)
callerSaves (FloatReg 6)      = True
#endif
#if defined(CALLER_SAVES_D1)
callerSaves (DoubleReg 1)     = True
#endif
#if defined(CALLER_SAVES_D2)
callerSaves (DoubleReg 2)     = True
#endif
#if defined(CALLER_SAVES_D3)
callerSaves (DoubleReg 3)     = True
#endif
#if defined(CALLER_SAVES_D4)
callerSaves (DoubleReg 4)     = True
#endif
#if defined(CALLER_SAVES_D5)
callerSaves (DoubleReg 5)     = True
#endif
#if defined(CALLER_SAVES_D6)
callerSaves (DoubleReg 6)     = True
#endif
#if defined(CALLER_SAVES_L1)
callerSaves (LongReg 1)       = True
#endif
#if defined(CALLER_SAVES_Sp)
callerSaves Sp                = True
#endif
#if defined(CALLER_SAVES_SpLim)
callerSaves SpLim             = True
#endif
#if defined(CALLER_SAVES_Hp)
callerSaves Hp                = True
#endif
#if defined(CALLER_SAVES_HpLim)
callerSaves HpLim             = True
#endif
#if defined(CALLER_SAVES_CCCS)
callerSaves CCCS              = True
#endif
#if defined(CALLER_SAVES_CurrentTSO)
callerSaves CurrentTSO        = True
#endif
#if defined(CALLER_SAVES_CurrentNursery)
callerSaves CurrentNursery    = True
#endif
callerSaves _                 = False

activeStgRegs :: [GlobalReg]
activeStgRegs = [
#if defined(REG_Base)
    BaseReg
#endif
#if defined(REG_Sp)
    ,Sp
#endif
#if defined(REG_Hp)
    ,Hp
#endif
#if defined(REG_R1)
    ,VanillaReg 1 VGcPtr
#endif
#if defined(REG_R2)
    ,VanillaReg 2 VGcPtr
#endif
#if defined(REG_R3)
    ,VanillaReg 3 VGcPtr
#endif
#if defined(REG_R4)
    ,VanillaReg 4 VGcPtr
#endif
#if defined(REG_R5)
    ,VanillaReg 5 VGcPtr
#endif
#if defined(REG_R6)
    ,VanillaReg 6 VGcPtr
#endif
#if defined(REG_R7)
    ,VanillaReg 7 VGcPtr
#endif
#if defined(REG_R8)
    ,VanillaReg 8 VGcPtr
#endif
#if defined(REG_R9)
    ,VanillaReg 9 VGcPtr
#endif
#if defined(REG_R10)
    ,VanillaReg 10 VGcPtr
#endif
#if defined(REG_SpLim)
    ,SpLim
#endif
#if MAX_REAL_XMM_REG != 0
#if defined(REG_F1)
    ,FloatReg 1
#endif
#if defined(REG_D1)
    ,DoubleReg 1
#endif
#if defined(REG_XMM1)
    ,XmmReg 1
#endif
#if defined(REG_YMM1)
    ,YmmReg 1
#endif
#if defined(REG_ZMM1)
    ,ZmmReg 1
#endif
#if defined(REG_F2)
    ,FloatReg 2
#endif
#if defined(REG_D2)
    ,DoubleReg 2
#endif
#if defined(REG_XMM2)
    ,XmmReg 2
#endif
#if defined(REG_YMM2)
    ,YmmReg 2
#endif
#if defined(REG_ZMM2)
    ,ZmmReg 2
#endif
#if defined(REG_F3)
    ,FloatReg 3
#endif
#if defined(REG_D3)
    ,DoubleReg 3
#endif
#if defined(REG_XMM3)
    ,XmmReg 3
#endif
#if defined(REG_YMM3)
    ,YmmReg 3
#endif
#if defined(REG_ZMM3)
    ,ZmmReg 3
#endif
#if defined(REG_F4)
    ,FloatReg 4
#endif
#if defined(REG_D4)
    ,DoubleReg 4
#endif
#if defined(REG_XMM4)
    ,XmmReg 4
#endif
#if defined(REG_YMM4)
    ,YmmReg 4
#endif
#if defined(REG_ZMM4)
    ,ZmmReg 4
#endif
#if defined(REG_F5)
    ,FloatReg 5
#endif
#if defined(REG_D5)
    ,DoubleReg 5
#endif
#if defined(REG_XMM5)
    ,XmmReg 5
#endif
#if defined(REG_YMM5)
    ,YmmReg 5
#endif
#if defined(REG_ZMM5)
    ,ZmmReg 5
#endif
#if defined(REG_F6)
    ,FloatReg 6
#endif
#if defined(REG_D6)
    ,DoubleReg 6
#endif
#if defined(REG_XMM6)
    ,XmmReg 6
#endif
#if defined(REG_YMM6)
    ,YmmReg 6
#endif
#if defined(REG_ZMM6)
    ,ZmmReg 6
#endif
#else /* MAX_REAL_XMM_REG == 0 */
#if defined(REG_F1)
    ,FloatReg 1
#endif
#if defined(REG_F2)
    ,FloatReg 2
#endif
#if defined(REG_F3)
    ,FloatReg 3
#endif
#if defined(REG_F4)
    ,FloatReg 4
#endif
#if defined(REG_F5)
    ,FloatReg 5
#endif
#if defined(REG_F6)
    ,FloatReg 6
#endif
#if defined(REG_D1)
    ,DoubleReg 1
#endif
#if defined(REG_D2)
    ,DoubleReg 2
#endif
#if defined(REG_D3)
    ,DoubleReg 3
#endif
#if defined(REG_D4)
    ,DoubleReg 4
#endif
#if defined(REG_D5)
    ,DoubleReg 5
#endif
#if defined(REG_D6)
    ,DoubleReg 6
#endif
#endif /* MAX_REAL_XMM_REG == 0 */
    ]

haveRegBase :: Bool
#if defined(REG_Base)
haveRegBase = True
#else
haveRegBase = False
#endif

--  | Returns 'Nothing' if this global register is not stored
-- in a real machine register, otherwise returns @'Just' reg@, where
-- reg is the machine register it is stored in.
globalRegMaybe :: GlobalReg -> Maybe RealReg
#if defined(MACHREGS_i386) || defined(MACHREGS_x86_64) \
    || defined(MACHREGS_powerpc) \
    || defined(MACHREGS_arm) || defined(MACHREGS_aarch64) \
    || defined(MACHREGS_s390x) || defined(MACHREGS_riscv64)
# if defined(REG_Base)
globalRegMaybe BaseReg                  = Just (RealRegSingle REG_Base)
# endif
# if defined(REG_R1)
globalRegMaybe (VanillaReg 1 _)         = Just (RealRegSingle REG_R1)
# endif
# if defined(REG_R2)
globalRegMaybe (VanillaReg 2 _)         = Just (RealRegSingle REG_R2)
# endif
# if defined(REG_R3)
globalRegMaybe (VanillaReg 3 _)         = Just (RealRegSingle REG_R3)
# endif
# if defined(REG_R4)
globalRegMaybe (VanillaReg 4 _)         = Just (RealRegSingle REG_R4)
# endif
# if defined(REG_R5)
globalRegMaybe (VanillaReg 5 _)         = Just (RealRegSingle REG_R5)
# endif
# if defined(REG_R6)
globalRegMaybe (VanillaReg 6 _)         = Just (RealRegSingle REG_R6)
# endif
# if defined(REG_R7)
globalRegMaybe (VanillaReg 7 _)         = Just (RealRegSingle REG_R7)
# endif
# if defined(REG_R8)
globalRegMaybe (VanillaReg 8 _)         = Just (RealRegSingle REG_R8)
# endif
# if defined(REG_R9)
globalRegMaybe (VanillaReg 9 _)         = Just (RealRegSingle REG_R9)
# endif
# if defined(REG_R10)
globalRegMaybe (VanillaReg 10 _)        = Just (RealRegSingle REG_R10)
# endif
# if defined(REG_F1)
globalRegMaybe (FloatReg 1)             = Just (RealRegSingle REG_F1)
# endif
# if defined(REG_F2)
globalRegMaybe (FloatReg 2)             = Just (RealRegSingle REG_F2)
# endif
# if defined(REG_F3)
globalRegMaybe (FloatReg 3)             = Just (RealRegSingle REG_F3)
# endif
# if defined(REG_F4)
globalRegMaybe (FloatReg 4)             = Just (RealRegSingle REG_F4)
# endif
# if defined(REG_F5)
globalRegMaybe (FloatReg 5)             = Just (RealRegSingle REG_F5)
# endif
# if defined(REG_F6)
globalRegMaybe (FloatReg 6)             = Just (RealRegSingle REG_F6)
# endif
# if defined(REG_D1)
globalRegMaybe (DoubleReg 1)            = Just (RealRegSingle REG_D1)
# endif
# if defined(REG_D2)
globalRegMaybe (DoubleReg 2)            = Just (RealRegSingle REG_D2)
# endif
# if defined(REG_D3)
globalRegMaybe (DoubleReg 3)            = Just (RealRegSingle REG_D3)
# endif
# if defined(REG_D4)
globalRegMaybe (DoubleReg 4)            = Just (RealRegSingle REG_D4)
# endif
# if defined(REG_D5)
globalRegMaybe (DoubleReg 5)            = Just (RealRegSingle REG_D5)
# endif
# if defined(REG_D6)
globalRegMaybe (DoubleReg 6)            = Just (RealRegSingle REG_D6)
# endif
# if MAX_REAL_XMM_REG != 0
#  if defined(REG_XMM1)
globalRegMaybe (XmmReg 1)               = Just (RealRegSingle REG_XMM1)
#  endif
#  if defined(REG_XMM2)
globalRegMaybe (XmmReg 2)               = Just (RealRegSingle REG_XMM2)
#  endif
#  if defined(REG_XMM3)
globalRegMaybe (XmmReg 3)               = Just (RealRegSingle REG_XMM3)
#  endif
#  if defined(REG_XMM4)
globalRegMaybe (XmmReg 4)               = Just (RealRegSingle REG_XMM4)
#  endif
#  if defined(REG_XMM5)
globalRegMaybe (XmmReg 5)               = Just (RealRegSingle REG_XMM5)
#  endif
#  if defined(REG_XMM6)
globalRegMaybe (XmmReg 6)               = Just (RealRegSingle REG_XMM6)
#  endif
# endif
# if defined(MAX_REAL_YMM_REG) && MAX_REAL_YMM_REG != 0
#  if defined(REG_YMM1)
globalRegMaybe (YmmReg 1)               = Just (RealRegSingle REG_YMM1)
#  endif
#  if defined(REG_YMM2)
globalRegMaybe (YmmReg 2)               = Just (RealRegSingle REG_YMM2)
#  endif
#  if defined(REG_YMM3)
globalRegMaybe (YmmReg 3)               = Just (RealRegSingle REG_YMM3)
#  endif
#  if defined(REG_YMM4)
globalRegMaybe (YmmReg 4)               = Just (RealRegSingle REG_YMM4)
#  endif
#  if defined(REG_YMM5)
globalRegMaybe (YmmReg 5)               = Just (RealRegSingle REG_YMM5)
#  endif
#  if defined(REG_YMM6)
globalRegMaybe (YmmReg 6)               = Just (RealRegSingle REG_YMM6)
#  endif
# endif
# if defined(MAX_REAL_ZMM_REG) && MAX_REAL_ZMM_REG != 0
#  if defined(REG_ZMM1)
globalRegMaybe (ZmmReg 1)               = Just (RealRegSingle REG_ZMM1)
#  endif
#  if defined(REG_ZMM2)
globalRegMaybe (ZmmReg 2)               = Just (RealRegSingle REG_ZMM2)
#  endif
#  if defined(REG_ZMM3)
globalRegMaybe (ZmmReg 3)               = Just (RealRegSingle REG_ZMM3)
#  endif
#  if defined(REG_ZMM4)
globalRegMaybe (ZmmReg 4)               = Just (RealRegSingle REG_ZMM4)
#  endif
#  if defined(REG_ZMM5)
globalRegMaybe (ZmmReg 5)               = Just (RealRegSingle REG_ZMM5)
#  endif
#  if defined(REG_ZMM6)
globalRegMaybe (ZmmReg 6)               = Just (RealRegSingle REG_ZMM6)
#  endif
# endif
# if defined(REG_Sp)
globalRegMaybe Sp                       = Just (RealRegSingle REG_Sp)
# endif
# if defined(REG_Lng1)
globalRegMaybe (LongReg 1)              = Just (RealRegSingle REG_Lng1)
# endif
# if defined(REG_Lng2)
globalRegMaybe (LongReg 2)              = Just (RealRegSingle REG_Lng2)
# endif
# if defined(REG_SpLim)
globalRegMaybe SpLim                    = Just (RealRegSingle REG_SpLim)
# endif
# if defined(REG_Hp)
globalRegMaybe Hp                       = Just (RealRegSingle REG_Hp)
# endif
# if defined(REG_HpLim)
globalRegMaybe HpLim                    = Just (RealRegSingle REG_HpLim)
# endif
# if defined(REG_CurrentTSO)
globalRegMaybe CurrentTSO               = Just (RealRegSingle REG_CurrentTSO)
# endif
# if defined(REG_CurrentNursery)
globalRegMaybe CurrentNursery           = Just (RealRegSingle REG_CurrentNursery)
# endif
# if defined(REG_MachSp)
globalRegMaybe MachSp                   = Just (RealRegSingle REG_MachSp)
# endif
globalRegMaybe _                        = Nothing
#elif defined(MACHREGS_NO_REGS)
globalRegMaybe _ = Nothing
#else
globalRegMaybe = panic "globalRegMaybe not defined for this platform"
#endif

freeReg :: RegNo -> Bool

#if defined(MACHREGS_i386) || defined(MACHREGS_x86_64)

# if defined(MACHREGS_i386)
freeReg esp = False -- %esp is the C stack pointer
freeReg esi = False -- See Note [esi/edi/ebp not allocatable]
freeReg edi = False
freeReg ebp = False
# endif
# if defined(MACHREGS_x86_64)
freeReg rsp = False  --        %rsp is the C stack pointer
# endif

{-
Note [esi/edi/ebp not allocatable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%esi is mapped to R1, so %esi would normally be allocatable while it
is not being used for R1.  However, %esi has no 8-bit version on x86,
and the linear register allocator is not sophisticated enough to
handle this irregularity (we need more RegClasses).  The
graph-colouring allocator also cannot handle this - it was designed
with more flexibility in mind, but the current implementation is
restricted to the same set of classes as the linear allocator.

Hence, on x86 esi, edi and ebp are treated as not allocatable.
-}

-- split patterns in two functions to prevent overlaps
freeReg r         = freeRegBase r

freeRegBase :: RegNo -> Bool
# if defined(REG_Base)
freeRegBase REG_Base  = False
# endif
# if defined(REG_Sp)
freeRegBase REG_Sp    = False
# endif
# if defined(REG_SpLim)
freeRegBase REG_SpLim = False
# endif
# if defined(REG_Hp)
freeRegBase REG_Hp    = False
# endif
# if defined(REG_HpLim)
freeRegBase REG_HpLim = False
# endif
-- All other regs are considered to be "free", because we can track
-- their liveness accurately.
freeRegBase _ = True

#elif defined(MACHREGS_powerpc)

freeReg 0 = False -- Used by code setting the back chain pointer
                  -- in stack reallocations on Linux.
                  -- Moreover r0 is not usable in all insns.
freeReg 1 = False -- The Stack Pointer
-- most ELF PowerPC OSes use r2 as a TOC pointer
freeReg 2 = False
freeReg 13 = False -- reserved for system thread ID on 64 bit
-- at least linux in -fPIC relies on r30 in PLT stubs
freeReg 30 = False
{- TODO: reserve r13 on 64 bit systems only and r30 on 32 bit respectively.
   For now we use r30 on 64 bit and r13 on 32 bit as a temporary register
   in stack handling code. See compiler/GHC/CmmToAsm/PPC/Instr.hs.

   Later we might want to reserve r13 and r30 only where it is required.
   Then use r12 as temporary register, which is also what the C ABI does.
-}

# if defined(REG_Base)
freeReg REG_Base  = False
# endif
# if defined(REG_Sp)
freeReg REG_Sp    = False
# endif
# if defined(REG_SpLim)
freeReg REG_SpLim = False
# endif
# if defined(REG_Hp)
freeReg REG_Hp    = False
# endif
# if defined(REG_HpLim)
freeReg REG_HpLim = False
# endif
freeReg _ = True

#elif defined(MACHREGS_aarch64)

-- stack pointer / zero reg
freeReg 31 = False
-- link register
freeReg 30 = False
-- frame pointer
freeReg 29 = False
-- ip0 -- used for spill offset computations
freeReg 16 = False

# if defined(REG_Base)
freeReg REG_Base  = False
# endif
# if defined(REG_Sp)
freeReg REG_Sp    = False
# endif
# if defined(REG_SpLim)
freeReg REG_SpLim = False
# endif
# if defined(REG_Hp)
freeReg REG_Hp    = False
# endif
# if defined(REG_HpLim)
freeReg REG_HpLim = False
# endif

# if defined(REG_R1)
freeReg REG_R1    = False
# endif
# if defined(REG_R2)
freeReg REG_R2    = False
# endif
# if defined(REG_R3)
freeReg REG_R3    = False
# endif
# if defined(REG_R4)
freeReg REG_R4    = False
# endif
# if defined(REG_R5)
freeReg REG_R5    = False
# endif
# if defined(REG_R6)
freeReg REG_R6    = False
# endif
# if defined(REG_R7)
freeReg REG_R7    = False
# endif
# if defined(REG_R8)
freeReg REG_R8    = False
# endif

# if defined(REG_F1)
freeReg REG_F1    = False
# endif
# if defined(REG_F2)
freeReg REG_F2    = False
# endif
# if defined(REG_F3)
freeReg REG_F3    = False
# endif
# if defined(REG_F4)
freeReg REG_F4    = False
# endif
# if defined(REG_F5)
freeReg REG_F5    = False
# endif
# if defined(REG_F6)
freeReg REG_F6    = False
# endif

# if defined(REG_D1)
freeReg REG_D1    = False
# endif
# if defined(REG_D2)
freeReg REG_D2    = False
# endif
# if defined(REG_D3)
freeReg REG_D3    = False
# endif
# if defined(REG_D4)
freeReg REG_D4    = False
# endif
# if defined(REG_D5)
freeReg REG_D5    = False
# endif
# if defined(REG_D6)
freeReg REG_D6    = False
# endif

freeReg _ = True

#else

freeReg = panic "freeReg not defined for this platform"

#endif

module X86.Regs (
        -- squeese functions for the graph allocator
        virtualRegSqueeze,
        realRegSqueeze,

        -- immediates
        Imm(..),
        strImmLit,
        litToImm,

        -- addressing modes
        AddrMode(..),
        addrOffset,

        -- registers
        spRel,
        argRegs,
        allArgRegs,
        allIntArgRegs,
        callClobberedRegs,
        instrClobberedRegs,
        allMachRegNos,
        classOfRealReg,
        showReg,

        -- machine specific
        EABase(..), EAIndex(..), addrModeRegs,

        eax, ebx, ecx, edx, esi, edi, ebp, esp,
        fake0, fake1, fake2, fake3, fake4, fake5, firstfake,

        rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp,
        r8,  r9,  r10, r11, r12, r13, r14, r15,
        xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7,
        xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15,
        xmm,

        ripRel,
        allFPArgRegs,

        allocatableRegs
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import CodeGen.Platform
import Reg
import RegClass

import Cmm
import CLabel           ( CLabel )
import DynFlags
import Outputable
import Platform
import FastTypes
import FastBool


-- | regSqueeze_class reg
--      Calculuate the maximum number of register colors that could be
--      denied to a node of this class due to having this reg
--      as a neighbour.
--
{-# INLINE virtualRegSqueeze #-}
virtualRegSqueeze :: RegClass -> VirtualReg -> FastInt

virtualRegSqueeze cls vr
 = case cls of
        RcInteger
         -> case vr of
                VirtualRegI{}           -> _ILIT(1)
                VirtualRegHi{}          -> _ILIT(1)
                _other                  -> _ILIT(0)

        RcDouble
         -> case vr of
                VirtualRegD{}           -> _ILIT(1)
                VirtualRegF{}           -> _ILIT(0)
                _other                  -> _ILIT(0)

        RcDoubleSSE
         -> case vr of
                VirtualRegSSE{}         -> _ILIT(1)
                _other                  -> _ILIT(0)

        _other -> _ILIT(0)

{-# INLINE realRegSqueeze #-}
realRegSqueeze :: RegClass -> RealReg -> FastInt
realRegSqueeze cls rr
 = case cls of
        RcInteger
         -> case rr of
                RealRegSingle regNo
                        | regNo < firstfake -> _ILIT(1)
                        | otherwise     -> _ILIT(0)

                RealRegPair{}           -> _ILIT(0)

        RcDouble
         -> case rr of
                RealRegSingle regNo
                        | regNo >= firstfake && regNo <= lastfake -> _ILIT(1)
                        | otherwise     -> _ILIT(0)

                RealRegPair{}           -> _ILIT(0)

        RcDoubleSSE
         -> case rr of
                RealRegSingle regNo | regNo >= firstxmm -> _ILIT(1)
                _otherwise                        -> _ILIT(0)

        _other -> _ILIT(0)

-- -----------------------------------------------------------------------------
-- Immediates

data Imm
  = ImmInt      Int
  | ImmInteger  Integer     -- Sigh.
  | ImmCLbl     CLabel      -- AbstractC Label (with baggage)
  | ImmLit      SDoc        -- Simple string
  | ImmIndex    CLabel Int
  | ImmFloat    Rational
  | ImmDouble   Rational
  | ImmConstantSum Imm Imm
  | ImmConstantDiff Imm Imm


strImmLit :: String -> Imm
strImmLit s = ImmLit (text s)


litToImm :: CmmLit -> Imm
litToImm (CmmInt i w)        = ImmInteger (narrowS w i)
                -- narrow to the width: a CmmInt might be out of
                -- range, but we assume that ImmInteger only contains
                -- in-range values.  A signed value should be fine here.
litToImm (CmmFloat f W32)    = ImmFloat f
litToImm (CmmFloat f W64)    = ImmDouble f
litToImm (CmmLabel l)        = ImmCLbl l
litToImm (CmmLabelOff l off) = ImmIndex l off
litToImm (CmmLabelDiffOff l1 l2 off)
                             = ImmConstantSum
                               (ImmConstantDiff (ImmCLbl l1) (ImmCLbl l2))
                               (ImmInt off)
litToImm _                   = panic "X86.Regs.litToImm: no match"

-- addressing modes ------------------------------------------------------------

data AddrMode
        = AddrBaseIndex EABase EAIndex Displacement
        | ImmAddr Imm Int

data EABase       = EABaseNone  | EABaseReg Reg | EABaseRip
data EAIndex      = EAIndexNone | EAIndex Reg Int
type Displacement = Imm


addrOffset :: AddrMode -> Int -> Maybe AddrMode
addrOffset addr off
  = case addr of
      ImmAddr i off0      -> Just (ImmAddr i (off0 + off))

      AddrBaseIndex r i (ImmInt n) -> Just (AddrBaseIndex r i (ImmInt (n + off)))
      AddrBaseIndex r i (ImmInteger n)
        -> Just (AddrBaseIndex r i (ImmInt (fromInteger (n + toInteger off))))

      AddrBaseIndex r i (ImmCLbl lbl)
        -> Just (AddrBaseIndex r i (ImmIndex lbl off))

      AddrBaseIndex r i (ImmIndex lbl ix)
        -> Just (AddrBaseIndex r i (ImmIndex lbl (ix+off)))

      _ -> Nothing  -- in theory, shouldn't happen


addrModeRegs :: AddrMode -> [Reg]
addrModeRegs (AddrBaseIndex b i _) =  b_regs ++ i_regs
  where
   b_regs = case b of { EABaseReg r -> [r]; _ -> [] }
   i_regs = case i of { EAIndex r _ -> [r]; _ -> [] }
addrModeRegs _ = []


-- registers -------------------------------------------------------------------

-- @spRel@ gives us a stack relative addressing mode for volatile
-- temporaries and for excess call arguments.  @fpRel@, where
-- applicable, is the same but for the frame pointer.


spRel :: DynFlags
      -> Int -- ^ desired stack offset in bytes, positive or negative
      -> AddrMode
spRel dflags n
 | target32Bit (targetPlatform dflags)
    = AddrBaseIndex (EABaseReg esp) EAIndexNone (ImmInt n)
 | otherwise
    = AddrBaseIndex (EABaseReg rsp) EAIndexNone (ImmInt n)

-- The register numbers must fit into 32 bits on x86, so that we can
-- use a Word32 to represent the set of free registers in the register
-- allocator.

firstfake, lastfake :: RegNo
firstfake = 16
lastfake  = 21

firstxmm :: RegNo
firstxmm  = 24

lastxmm :: Platform -> RegNo
lastxmm platform
 | target32Bit platform = 31
 | otherwise            = 39

lastint :: Platform -> RegNo
lastint platform
 | target32Bit platform = 7 -- not %r8..%r15
 | otherwise            = 15

intregnos :: Platform -> [RegNo]
intregnos platform = [0 .. lastint platform]

fakeregnos :: [RegNo]
fakeregnos  = [firstfake .. lastfake]

xmmregnos :: Platform -> [RegNo]
xmmregnos platform = [firstxmm  .. lastxmm platform]

floatregnos :: Platform -> [RegNo]
floatregnos platform = fakeregnos ++ xmmregnos platform


-- argRegs is the set of regs which are read for an n-argument call to C.
-- For archs which pass all args on the stack (x86), is empty.
-- Sparc passes up to the first 6 args in regs.
argRegs :: RegNo -> [Reg]
argRegs _       = panic "MachRegs.argRegs(x86): should not be used!"

-- | The complete set of machine registers.
allMachRegNos :: Platform -> [RegNo]
allMachRegNos platform = intregnos platform ++ floatregnos platform

-- | Take the class of a register.
{-# INLINE classOfRealReg #-}
classOfRealReg :: Platform -> RealReg -> RegClass
-- On x86, we might want to have an 8-bit RegClass, which would
-- contain just regs 1-4 (the others don't have 8-bit versions).
-- However, we can get away without this at the moment because the
-- only allocatable integer regs are also 8-bit compatible (1, 3, 4).
classOfRealReg platform reg
 = case reg of
        RealRegSingle i
          | i <= lastint platform -> RcInteger
          | i <= lastfake         -> RcDouble
          | otherwise             -> RcDoubleSSE

        RealRegPair{}   -> panic "X86.Regs.classOfRealReg: RegPairs on this arch"

-- | Get the name of the register with this number.
showReg :: Platform -> RegNo -> String
showReg platform n
        | n >= firstxmm  = "%xmm" ++ show (n-firstxmm)
        | n >= firstfake = "%fake" ++ show (n-firstfake)
        | n >= 8         = "%r" ++ show n
        | otherwise      = regNames platform !! n

regNames :: Platform -> [String]
regNames platform
    = if target32Bit platform
      then ["%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi", "%ebp", "%esp"]
      else ["%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%rbp", "%rsp"]



-- machine specific ------------------------------------------------------------


{-
Intel x86 architecture:
- All registers except 7 (esp) are available for use.
- Only ebx, esi, edi and esp are available across a C call (they are callee-saves).
- Registers 0-7 have 16-bit counterparts (ax, bx etc.)
- Registers 0-3 have 8 bit counterparts (ah, bh etc.)
- Registers fake0..fake5 are fakes; we pretend x86 has 6 conventionally-addressable
  fp registers, and 3-operand insns for them, and we translate this into
  real stack-based x86 fp code after register allocation.

The fp registers are all Double registers; we don't have any RcFloat class
regs.  @regClass@ barfs if you give it a VirtualRegF, and mkVReg above should
never generate them.
-}

fake0, fake1, fake2, fake3, fake4, fake5,
       eax, ebx, ecx, edx, esp, ebp, esi, edi :: Reg

eax   = regSingle 0
ebx   = regSingle 1
ecx   = regSingle 2
edx   = regSingle 3
esi   = regSingle 4
edi   = regSingle 5
ebp   = regSingle 6
esp   = regSingle 7
fake0 = regSingle 16
fake1 = regSingle 17
fake2 = regSingle 18
fake3 = regSingle 19
fake4 = regSingle 20
fake5 = regSingle 21



{-
AMD x86_64 architecture:
- All 16 integer registers are addressable as 8, 16, 32 and 64-bit values:

  8     16    32    64
  ---------------------
  al    ax    eax   rax
  bl    bx    ebx   rbx
  cl    cx    ecx   rcx
  dl    dx    edx   rdx
  sil   si    esi   rsi
  dil   si    edi   rdi
  bpl   bp    ebp   rbp
  spl   sp    esp   rsp
  r10b  r10w  r10d  r10
  r11b  r11w  r11d  r11
  r12b  r12w  r12d  r12
  r13b  r13w  r13d  r13
  r14b  r14w  r14d  r14
  r15b  r15w  r15d  r15
-}

rax, rbx, rcx, rdx, rsp, rbp, rsi, rdi,
  r8, r9, r10, r11, r12, r13, r14, r15,
  xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7,
  xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15 :: Reg

rax   = regSingle 0
rbx   = regSingle 1
rcx   = regSingle 2
rdx   = regSingle 3
rsi   = regSingle 4
rdi   = regSingle 5
rbp   = regSingle 6
rsp   = regSingle 7
r8    = regSingle 8
r9    = regSingle 9
r10   = regSingle 10
r11   = regSingle 11
r12   = regSingle 12
r13   = regSingle 13
r14   = regSingle 14
r15   = regSingle 15
xmm0  = regSingle 24
xmm1  = regSingle 25
xmm2  = regSingle 26
xmm3  = regSingle 27
xmm4  = regSingle 28
xmm5  = regSingle 29
xmm6  = regSingle 30
xmm7  = regSingle 31
xmm8  = regSingle 32
xmm9  = regSingle 33
xmm10 = regSingle 34
xmm11 = regSingle 35
xmm12 = regSingle 36
xmm13 = regSingle 37
xmm14 = regSingle 38
xmm15 = regSingle 39

ripRel :: Displacement -> AddrMode
ripRel imm      = AddrBaseIndex EABaseRip EAIndexNone imm


 -- so we can re-use some x86 code:
{-
eax = rax
ebx = rbx
ecx = rcx
edx = rdx
esi = rsi
edi = rdi
ebp = rbp
esp = rsp
-}

xmm :: RegNo -> Reg
xmm n = regSingle (firstxmm+n)




-- | these are the regs which we cannot assume stay alive over a C call.
callClobberedRegs       :: Platform -> [Reg]
-- caller-saves registers
callClobberedRegs platform
 | target32Bit platform = [eax,ecx,edx] ++ map regSingle (floatregnos platform)
 | otherwise
    -- all xmm regs are caller-saves
    -- caller-saves registers
    = [rax,rcx,rdx,rsi,rdi,r8,r9,r10,r11]
   ++ map regSingle (floatregnos platform)

allArgRegs :: Platform -> [(Reg, Reg)]
allArgRegs platform
 | platformOS platform == OSMinGW32 = zip [rcx,rdx,r8,r9]
                                          (map regSingle [firstxmm ..])
 | otherwise = panic "X86.Regs.allArgRegs: not defined for this arch"

allIntArgRegs :: Platform -> [Reg]
allIntArgRegs platform
 | (platformOS platform == OSMinGW32) || target32Bit platform
    = panic "X86.Regs.allIntArgRegs: not defined for this platform"
 | otherwise = [rdi,rsi,rdx,rcx,r8,r9]

allFPArgRegs :: Platform -> [Reg]
allFPArgRegs platform
 | platformOS platform == OSMinGW32
    = panic "X86.Regs.allFPArgRegs: not defined for this platform"
 | otherwise = map regSingle [firstxmm .. firstxmm+7]

-- Machine registers which might be clobbered by instructions that
-- generate results into fixed registers, or need arguments in a fixed
-- register.
instrClobberedRegs :: Platform -> [Reg]
instrClobberedRegs platform
 | target32Bit platform = [ eax, ecx, edx ]
 | otherwise            = [ rax, rcx, rdx ]

--

-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
-- i.e., these are the regs for which we are prepared to allow the
-- register allocator to attempt to map VRegs to.
allocatableRegs :: Platform -> [RealReg]
allocatableRegs platform
   = let isFree i = isFastTrue (freeReg platform i)
     in  map RealRegSingle $ filter isFree (allMachRegNos platform)


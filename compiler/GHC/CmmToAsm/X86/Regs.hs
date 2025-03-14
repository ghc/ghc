

module GHC.CmmToAsm.X86.Regs (
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

        -- machine specific
        EABase(..), EAIndex(..), addrModeRegs,

        eax, ebx, ecx, edx, esi, edi, ebp, esp,


        rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp,
        r8,  r9,  r10, r11, r12, r13, r14, r15,
        lastint,
        xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7,
        xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15,
        xmm,
        firstxmm, lastxmm,
        intregnos, xmmregnos,

        ripRel,
        allFPArgRegs,

        allocatableRegs
)

where

import GHC.Prelude
import GHC.Data.FastString

import GHC.Platform.Regs
import GHC.Platform.Reg
import GHC.Platform.Reg.Class.Unified

import GHC.Cmm
import GHC.Cmm.CLabel           ( CLabel )
import GHC.Utils.Panic
import GHC.Platform

-- | regSqueeze_class reg
--      Calculate the maximum number of register colors that could be
--      denied to a node of this class due to having this reg
--      as a neighbour.
--
{-# INLINE virtualRegSqueeze #-}
virtualRegSqueeze :: RegClass -> VirtualReg -> Int

virtualRegSqueeze cls vr
 = case cls of
        RcInteger
         -> case vr of
                VirtualRegI{}           -> 1
                VirtualRegHi{}          -> 1
                _other                  -> 0

        RcFloatOrVector
         -> case vr of
                VirtualRegD{}           -> 1
                VirtualRegV128{}        -> 1
                _other                  -> 0


{-# INLINE realRegSqueeze #-}
realRegSqueeze :: RegClass -> RealReg -> Int
realRegSqueeze cls rr
 = case cls of
        RcInteger
         -> case rr of
                RealRegSingle regNo
                        | regNo < firstxmm -> 1
                        | otherwise     -> 0

        RcFloatOrVector
         -> case rr of
                RealRegSingle regNo
                        | regNo >= firstxmm  -> 1
                        | otherwise     -> 0

-- -----------------------------------------------------------------------------
-- Immediates

data Imm
  = ImmInt      Int
  | ImmInteger  Integer     -- Sigh.
  | ImmCLbl     CLabel      -- AbstractC Label (with baggage)
  | ImmLit      FastString
  | ImmIndex    CLabel Int
  | ImmFloat    Rational
  | ImmDouble   Rational
  | ImmConstantSum Imm Imm
  | ImmConstantDiff Imm Imm

strImmLit :: FastString -> Imm
strImmLit s = ImmLit s


litToImm :: CmmLit -> Imm
litToImm (CmmInt i w)        = ImmInteger (narrowS w i)
                -- narrow to the width: a CmmInt might be out of
                -- range, but we assume that ImmInteger only contains
                -- in-range values.  A signed value should be fine here.
litToImm (CmmFloat f W32)    = ImmFloat f
litToImm (CmmFloat f W64)    = ImmDouble f
litToImm (CmmLabel l)        = ImmCLbl l
litToImm (CmmLabelOff l off) = ImmIndex l off
litToImm (CmmLabelDiffOff l1 l2 off _)
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


spRel :: Platform
      -> Int -- ^ desired stack offset in bytes, positive or negative
      -> AddrMode
spRel platform n
 | target32Bit platform
    = AddrBaseIndex (EABaseReg esp) EAIndexNone (ImmInt n)
 | otherwise
    = AddrBaseIndex (EABaseReg rsp) EAIndexNone (ImmInt n)

-- The register numbers must fit into 32 bits on x86, so that we can
-- use a Word32 to represent the set of free registers in the register
-- allocator.


firstxmm :: RegNo
firstxmm  = 16

--  on 32bit platformOSs, only the first 8 XMM/YMM/ZMM registers are available
lastxmm :: Platform -> RegNo
lastxmm platform
 | target32Bit platform = firstxmm + 7  -- xmm0 - xmmm7
 | otherwise            = firstxmm + 15 -- xmm0 -xmm15

lastint :: Platform -> RegNo
lastint platform
 | target32Bit platform = 7 -- not %r8..%r15
 | otherwise            = 15

intregnos :: Platform -> [RegNo]
intregnos platform = [0 .. lastint platform]


xmmregnos :: Platform -> [RegNo]
xmmregnos platform = [firstxmm  .. lastxmm platform]

floatregnos :: Platform -> [RegNo]
floatregnos platform = xmmregnos platform

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
            | i <= lastxmm platform -> RcFloatOrVector
            | otherwise             -> panic "X86.Reg.classOfRealReg registerSingle too high"

-- machine specific ------------------------------------------------------------


{-
Intel x86 architecture:
- All registers except 7 (esp) are available for use.
- Only ebx, esi, edi and esp are available across a C call (they are callee-saves).
- Registers 0-7 have 16-bit counterparts (ax, bx etc.)
- Registers 0-3 have 8 bit counterparts (ah, bh etc.)

The fp registers support Float, Doubles and vectors of those, as well
as vectors of integer values.
-}


eax, ebx, ecx, edx, esp, ebp, esi, edi :: Reg

eax   = regSingle 0
ebx   = regSingle 1
ecx   = regSingle 2
edx   = regSingle 3
esi   = regSingle 4
edi   = regSingle 5
ebp   = regSingle 6
esp   = regSingle 7


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
xmm0  = regSingle 16
xmm1  = regSingle 17
xmm2  = regSingle 18
xmm3  = regSingle 19
xmm4  = regSingle 20
xmm5  = regSingle 21
xmm6  = regSingle 22
xmm7  = regSingle 23
xmm8  = regSingle 24
xmm9  = regSingle 25
xmm10 = regSingle 26
xmm11 = regSingle 27
xmm12 = regSingle 28
xmm13 = regSingle 29
xmm14 = regSingle 30
xmm15 = regSingle 31

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
 | platformOS platform == OSMinGW32
   = [rax,rcx,rdx,r8,r9,r10,r11]
   -- Only xmm0-5 are caller-saves registers on 64-bit windows.
   -- For details check the Win64 ABI:
   -- https://docs.microsoft.com/en-us/cpp/build/x64-software-conventions
   ++ map xmm [0 .. 5]
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


-- | on 64bit platforms we pass the first 8 float/double arguments
-- in the xmm registers.
allFPArgRegs :: Platform -> [Reg]
allFPArgRegs platform
 | platformOS platform == OSMinGW32
    = panic "X86.Regs.allFPArgRegs: not defined for this platform"
 | otherwise = map regSingle [firstxmm .. firstxmm + 7 ]


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
   = let isFree i = freeReg platform i
     in  map RealRegSingle $ filter isFree (allMachRegNos platform)


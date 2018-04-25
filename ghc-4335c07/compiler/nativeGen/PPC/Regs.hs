{-# LANGUAGE CPP #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1994-2004
--
-- -----------------------------------------------------------------------------

module PPC.Regs (
        -- squeeze functions
        virtualRegSqueeze,
        realRegSqueeze,

        mkVirtualReg,
        regDotColor,

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
        callClobberedRegs,
        allMachRegNos,
        classOfRealReg,
        showReg,

        -- machine specific
        allFPArgRegs,
        fits16Bits,
        makeImmediate,
        fReg,
        r0, sp, toc, r3, r4, r11, r12, r27, r28, r30,
        tmpReg,
        f1, f20, f21,

        allocatableRegs

)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import GhcPrelude

import Reg
import RegClass
import Format

import Cmm
import CLabel           ( CLabel )
import Unique

import CodeGen.Platform
import DynFlags
import Outputable
import Platform

import Data.Word        ( Word8, Word16, Word32, Word64 )
import Data.Int         ( Int8, Int16, Int32, Int64 )


-- squeese functions for the graph allocator -----------------------------------

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

        RcDouble
         -> case vr of
                VirtualRegD{}           -> 1
                VirtualRegF{}           -> 0
                _other                  -> 0

        _other -> 0

{-# INLINE realRegSqueeze #-}
realRegSqueeze :: RegClass -> RealReg -> Int
realRegSqueeze cls rr
 = case cls of
        RcInteger
         -> case rr of
                RealRegSingle regNo
                        | regNo < 32    -> 1     -- first fp reg is 32
                        | otherwise     -> 0

                RealRegPair{}           -> 0

        RcDouble
         -> case rr of
                RealRegSingle regNo
                        | regNo < 32    -> 0
                        | otherwise     -> 1

                RealRegPair{}           -> 0

        _other -> 0

mkVirtualReg :: Unique -> Format -> VirtualReg
mkVirtualReg u format
   | not (isFloatFormat format) = VirtualRegI u
   | otherwise
   = case format of
        FF32    -> VirtualRegD u
        FF64    -> VirtualRegD u
        _       -> panic "mkVirtualReg"

regDotColor :: RealReg -> SDoc
regDotColor reg
 = case classOfRealReg reg of
        RcInteger       -> text "blue"
        RcFloat         -> text "red"
        RcDouble        -> text "green"
        RcDoubleSSE     -> text "yellow"


-- immediates ------------------------------------------------------------------
data Imm
        = ImmInt        Int
        | ImmInteger    Integer     -- Sigh.
        | ImmCLbl       CLabel      -- AbstractC Label (with baggage)
        | ImmLit        SDoc        -- Simple string
        | ImmIndex    CLabel Int
        | ImmFloat      Rational
        | ImmDouble     Rational
        | ImmConstantSum Imm Imm
        | ImmConstantDiff Imm Imm
        | LO Imm
        | HI Imm
        | HA Imm        {- high halfword adjusted -}
        | HIGHERA Imm
        | HIGHESTA Imm


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
litToImm _                   = panic "PPC.Regs.litToImm: no match"


-- addressing modes ------------------------------------------------------------

data AddrMode
        = AddrRegReg    Reg Reg
        | AddrRegImm    Reg Imm


addrOffset :: AddrMode -> Int -> Maybe AddrMode
addrOffset addr off
  = case addr of
      AddrRegImm r (ImmInt n)
       | fits16Bits n2 -> Just (AddrRegImm r (ImmInt n2))
       | otherwise     -> Nothing
       where n2 = n + off

      AddrRegImm r (ImmInteger n)
       | fits16Bits n2 -> Just (AddrRegImm r (ImmInt (fromInteger n2)))
       | otherwise     -> Nothing
       where n2 = n + toInteger off

      _ -> Nothing


-- registers -------------------------------------------------------------------
-- @spRel@ gives us a stack relative addressing mode for volatile
-- temporaries and for excess call arguments.  @fpRel@, where
-- applicable, is the same but for the frame pointer.

spRel :: DynFlags
      -> Int    -- desired stack offset in words, positive or negative
      -> AddrMode

spRel dflags n = AddrRegImm sp (ImmInt (n * wORD_SIZE dflags))


-- argRegs is the set of regs which are read for an n-argument call to C.
-- For archs which pass all args on the stack (x86), is empty.
-- Sparc passes up to the first 6 args in regs.
argRegs :: RegNo -> [Reg]
argRegs 0 = []
argRegs 1 = map regSingle [3]
argRegs 2 = map regSingle [3,4]
argRegs 3 = map regSingle [3..5]
argRegs 4 = map regSingle [3..6]
argRegs 5 = map regSingle [3..7]
argRegs 6 = map regSingle [3..8]
argRegs 7 = map regSingle [3..9]
argRegs 8 = map regSingle [3..10]
argRegs _ = panic "MachRegs.argRegs(powerpc): don't know about >8 arguments!"


allArgRegs :: [Reg]
allArgRegs = map regSingle [3..10]


-- these are the regs which we cannot assume stay alive over a C call.
callClobberedRegs :: Platform -> [Reg]
callClobberedRegs platform
  = case platformOS platform of
    OSAIX    -> map regSingle (0:[2..12] ++ map fReg [0..13])
    OSDarwin -> map regSingle (0:[2..12] ++ map fReg [0..13])
    OSLinux  -> map regSingle (0:[2..13] ++ map fReg [0..13])
    _        -> panic "PPC.Regs.callClobberedRegs: not defined for this architecture"


allMachRegNos   :: [RegNo]
allMachRegNos   = [0..63]


{-# INLINE classOfRealReg      #-}
classOfRealReg :: RealReg -> RegClass
classOfRealReg (RealRegSingle i)
        | i < 32        = RcInteger
        | otherwise     = RcDouble

classOfRealReg (RealRegPair{})
        = panic "regClass(ppr): no reg pairs on this architecture"

showReg :: RegNo -> String
showReg n
    | n >= 0 && n <= 31   = "%r" ++ show n
    | n >= 32 && n <= 63  = "%f" ++ show (n - 32)
    | otherwise           = "%unknown_powerpc_real_reg_" ++ show n



-- machine specific ------------------------------------------------------------

allFPArgRegs :: Platform -> [Reg]
allFPArgRegs platform
    = case platformOS platform of
      OSAIX    -> map (regSingle . fReg) [1..13]
      OSDarwin -> map (regSingle . fReg) [1..13]
      OSLinux  -> case platformArch platform of
        ArchPPC      -> map (regSingle . fReg) [1..8]
        ArchPPC_64 _ -> map (regSingle . fReg) [1..13]
        _            -> panic "PPC.Regs.allFPArgRegs: unknown PPC Linux"
      _        -> panic "PPC.Regs.allFPArgRegs: not defined for this architecture"

fits16Bits :: Integral a => a -> Bool
fits16Bits x = x >= -32768 && x < 32768

makeImmediate :: Integral a => Width -> Bool -> a -> Maybe Imm
makeImmediate rep signed x = fmap ImmInt (toI16 rep signed)
    where
        narrow W64 False = fromIntegral (fromIntegral x :: Word64)
        narrow W32 False = fromIntegral (fromIntegral x :: Word32)
        narrow W16 False = fromIntegral (fromIntegral x :: Word16)
        narrow W8  False = fromIntegral (fromIntegral x :: Word8)
        narrow W64 True  = fromIntegral (fromIntegral x :: Int64)
        narrow W32 True  = fromIntegral (fromIntegral x :: Int32)
        narrow W16 True  = fromIntegral (fromIntegral x :: Int16)
        narrow W8  True  = fromIntegral (fromIntegral x :: Int8)
        narrow _   _     = panic "PPC.Regs.narrow: no match"

        narrowed = narrow rep signed

        toI16 W32 True
            | narrowed >= -32768 && narrowed < 32768 = Just narrowed
            | otherwise = Nothing
        toI16 W32 False
            | narrowed >= 0 && narrowed < 65536 = Just narrowed
            | otherwise = Nothing
        toI16 W64 True
            | narrowed >= -32768 && narrowed < 32768 = Just narrowed
            | otherwise = Nothing
        toI16 W64 False
            | narrowed >= 0 && narrowed < 65536 = Just narrowed
            | otherwise = Nothing
        toI16 _ _  = Just narrowed


{-
The PowerPC has 64 registers of interest; 32 integer registers and 32 floating
point registers.
-}

fReg :: Int -> RegNo
fReg x = (32 + x)

r0, sp, toc, r3, r4, r11, r12, r27, r28, r30, f1, f20, f21 :: Reg
r0      = regSingle 0
sp      = regSingle 1
toc     = regSingle 2
r3      = regSingle 3
r4      = regSingle 4
r11     = regSingle 11
r12     = regSingle 12
r27     = regSingle 27
r28     = regSingle 28
r30     = regSingle 30
f1      = regSingle $ fReg 1
f20     = regSingle $ fReg 20
f21     = regSingle $ fReg 21

-- allocatableRegs is allMachRegNos with the fixed-use regs removed.
-- i.e., these are the regs for which we are prepared to allow the
-- register allocator to attempt to map VRegs to.
allocatableRegs :: Platform -> [RealReg]
allocatableRegs platform
   = let isFree i = freeReg platform i
     in  map RealRegSingle $ filter isFree allMachRegNos

-- temporary register for compiler use
tmpReg :: Platform -> Reg
tmpReg platform =
       case platformArch platform of
       ArchPPC      -> regSingle 13
       ArchPPC_64 _ -> regSingle 30
       _            -> panic "PPC.Regs.tmpReg: unknowm arch"

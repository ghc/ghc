-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1994-2004
--
-- -----------------------------------------------------------------------------

module SPARC.Regs (
        -- registers
        showReg,
        virtualRegSqueeze,
        realRegSqueeze,
        classOfRealReg,
        allRealRegs,

        -- machine specific info
        gReg, iReg, lReg, oReg, fReg,
        fp, sp, g0, g1, g2, o0, o1, f0, f1, f6, f8, f22, f26, f27,

        -- allocatable
        allocatableRegs,

        -- args
        argRegs,
        allArgRegs,
        callClobberedRegs,

        --
        mkVirtualReg,
        regDotColor
)

where


import CodeGen.Platform.SPARC
import Reg
import RegClass
import Size

import Unique
import Outputable
import FastTypes
import FastBool

{-
        The SPARC has 64 registers of interest; 32 integer registers and 32
        floating point registers.  The mapping of STG registers to SPARC
        machine registers is defined in StgRegs.h.  We are, of course,
        prepared for any eventuality.

        The whole fp-register pairing thing on sparcs is a huge nuisance.  See
        includes/stg/MachRegs.h for a description of what's going on
        here.
-}


-- | Get the standard name for the register with this number.
showReg :: RegNo -> String
showReg n
        | n >= 0  && n < 8   = "%g" ++ show n
        | n >= 8  && n < 16  = "%o" ++ show (n-8)
        | n >= 16 && n < 24  = "%l" ++ show (n-16)
        | n >= 24 && n < 32  = "%i" ++ show (n-24)
        | n >= 32 && n < 64  = "%f" ++ show (n-32)
        | otherwise          = panic "SPARC.Regs.showReg: unknown sparc register"


-- Get the register class of a certain real reg
classOfRealReg :: RealReg -> RegClass
classOfRealReg reg
 = case reg of
        RealRegSingle i
                | i < 32        -> RcInteger
                | otherwise     -> RcFloat

        RealRegPair{}           -> RcDouble


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

        RcFloat
         -> case vr of
                VirtualRegF{}           -> _ILIT(1)
                VirtualRegD{}           -> _ILIT(2)
                _other                  -> _ILIT(0)

        RcDouble
         -> case vr of
                VirtualRegF{}           -> _ILIT(1)
                VirtualRegD{}           -> _ILIT(1)
                _other                  -> _ILIT(0)

        _other -> _ILIT(0)

{-# INLINE realRegSqueeze #-}
realRegSqueeze :: RegClass -> RealReg -> FastInt

realRegSqueeze cls rr
 = case cls of
        RcInteger
         -> case rr of
                RealRegSingle regNo
                        | regNo < 32    -> _ILIT(1)
                        | otherwise     -> _ILIT(0)

                RealRegPair{}           -> _ILIT(0)

        RcFloat
         -> case rr of
                RealRegSingle regNo
                        | regNo < 32    -> _ILIT(0)
                        | otherwise     -> _ILIT(1)

                RealRegPair{}           -> _ILIT(2)

        RcDouble
         -> case rr of
                RealRegSingle regNo
                        | regNo < 32    -> _ILIT(0)
                        | otherwise     -> _ILIT(1)

                RealRegPair{}           -> _ILIT(1)

        _other -> _ILIT(0)

-- | All the allocatable registers in the machine,
--      including register pairs.
allRealRegs :: [RealReg]
allRealRegs
        =  [ (RealRegSingle i)          | i <- [0..63] ]
        ++ [ (RealRegPair   i (i+1))    | i <- [32, 34 .. 62 ] ]


-- | Get the regno for this sort of reg
gReg, lReg, iReg, oReg, fReg :: Int -> RegNo

gReg x  = x             -- global regs
oReg x  = (8 + x)       -- output regs
lReg x  = (16 + x)      -- local regs
iReg x  = (24 + x)      -- input regs
fReg x  = (32 + x)      -- float regs


-- | Some specific regs used by the code generator.
g0, g1, g2, fp, sp, o0, o1, f0, f1, f6, f8, f22, f26, f27 :: Reg

f6  = RegReal (RealRegSingle (fReg 6))
f8  = RegReal (RealRegSingle (fReg 8))
f22 = RegReal (RealRegSingle (fReg 22))
f26 = RegReal (RealRegSingle (fReg 26))
f27 = RegReal (RealRegSingle (fReg 27))

-- g0 is always zero, and writes to it vanish.
g0  = RegReal (RealRegSingle (gReg 0))
g1  = RegReal (RealRegSingle (gReg 1))
g2  = RegReal (RealRegSingle (gReg 2))

-- FP, SP, int and float return (from C) regs.
fp  = RegReal (RealRegSingle (iReg 6))
sp  = RegReal (RealRegSingle (oReg 6))
o0  = RegReal (RealRegSingle (oReg 0))
o1  = RegReal (RealRegSingle (oReg 1))
f0  = RegReal (RealRegSingle (fReg 0))
f1  = RegReal (RealRegSingle (fReg 1))

-- | Produce the second-half-of-a-double register given the first half.
{-
fPair :: Reg -> Maybe Reg
fPair (RealReg n)
        | n >= 32 && n `mod` 2 == 0  = Just (RealReg (n+1))

fPair (VirtualRegD u)
        = Just (VirtualRegHi u)

fPair reg
        = trace ("MachInstrs.fPair: can't get high half of supposed double reg " ++ showPpr reg)
                Nothing
-}


-- | All the regs that the register allocator can allocate to,
--      with the the fixed use regs removed.
--
allocatableRegs :: [RealReg]
allocatableRegs
   = let isFree rr
           = case rr of
                RealRegSingle r
                        -> isFastTrue (freeReg r)

                RealRegPair   r1 r2
                        -> isFastTrue (freeReg r1)
                        && isFastTrue (freeReg r2)

     in filter isFree allRealRegs


-- | The registers to place arguments for function calls,
--      for some number of arguments.
--
argRegs :: RegNo -> [Reg]
argRegs r
 = case r of
        0       -> []
        1       -> map (RegReal . RealRegSingle . oReg) [0]
        2       -> map (RegReal . RealRegSingle . oReg) [0,1]
        3       -> map (RegReal . RealRegSingle . oReg) [0,1,2]
        4       -> map (RegReal . RealRegSingle . oReg) [0,1,2,3]
        5       -> map (RegReal . RealRegSingle . oReg) [0,1,2,3,4]
        6       -> map (RegReal . RealRegSingle . oReg) [0,1,2,3,4,5]
        _       -> panic "MachRegs.argRegs(sparc): don't know about >6 arguments!"


-- | All all the regs that could possibly be returned by argRegs
--
allArgRegs :: [Reg]
allArgRegs
        = map (RegReal . RealRegSingle) [oReg i | i <- [0..5]]


-- These are the regs that we cannot assume stay alive over a C call.
--      TODO: Why can we assume that o6 isn't clobbered? -- BL 2009/02
--
callClobberedRegs :: [Reg]
callClobberedRegs
        = map (RegReal . RealRegSingle)
                (  oReg 7 :
                  [oReg i | i <- [0..5]] ++
                  [gReg i | i <- [1..7]] ++
                  [fReg i | i <- [0..31]] )



-- | Make a virtual reg with this size.
mkVirtualReg :: Unique -> Size -> VirtualReg
mkVirtualReg u size
        | not (isFloatSize size)
        = VirtualRegI u

        | otherwise
        = case size of
                FF32    -> VirtualRegF u
                FF64    -> VirtualRegD u
                _       -> panic "mkVReg"


regDotColor :: RealReg -> SDoc
regDotColor reg
 = case classOfRealReg reg of
        RcInteger       -> text "blue"
        RcFloat         -> text "red"
        _other          -> text "green"

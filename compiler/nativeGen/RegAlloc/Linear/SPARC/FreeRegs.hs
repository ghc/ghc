
-- | Free regs map for SPARC
module RegAlloc.Linear.SPARC.FreeRegs
where

import SPARC.Regs
import SPARC.RegPlate
import RegClass
import Reg

import Outputable
import FastBool

import Data.Word
import Data.Bits
import Data.List


--------------------------------------------------------------------------------
-- SPARC is like PPC, except for twinning of floating point regs.
--	When we allocate a double reg we must take an even numbered
--	float reg, as well as the one after it.


-- Holds bitmaps showing what registers are currently allocated.
--	The float and double reg bitmaps overlap, but we only alloc
--	float regs into the float map, and double regs into the double map.
--
--	Free regs have a bit set in the corresponding bitmap.
--
data FreeRegs 
	= FreeRegs 
		!Word32 	-- int    reg bitmap	regs  0..31
		!Word32 	-- float  reg bitmap	regs 32..63
		!Word32		-- double reg bitmap	regs 32..63
	deriving( Show )


-- | A reg map where no regs are free to be allocated.
noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0 0 0


-- | The initial set of free regs.
--	Don't treat the top half of reg pairs we're using as doubles as being free.
initFreeRegs :: FreeRegs
initFreeRegs 
 = 	regs
 where	
--	freeDouble	= getFreeRegs RcDouble regs
	regs		= foldr releaseReg noFreeRegs allocable
	allocable	= allocatableRegs \\ doublePairs
	doublePairs	= [43, 45, 47, 49, 51, 53]

			
-- | Get all the free registers of this class.
getFreeRegs :: RegClass -> FreeRegs -> [RegNo]	-- lazilly
getFreeRegs cls (FreeRegs g f d)
	| RcInteger <- cls = go g 1 0
	| RcFloat   <- cls = go f 1 32
	| RcDouble  <- cls = go d 1 32
	| otherwise = pprPanic "RegAllocLinear.getFreeRegs: Bad register class " (ppr cls)
	where
		go _ 0 _ = []
	        go x m i | x .&. m /= 0 = i : (go x (m `shiftL` 1) $! i+1)
        	         | otherwise    = go x (m `shiftL` 1) $! i+1
{-
showFreeRegs :: FreeRegs -> String
showFreeRegs regs
 	=  "FreeRegs\n"
	++ "    integer: " ++ (show $ getFreeRegs RcInteger regs)	++ "\n"
	++ "      float: " ++ (show $ getFreeRegs RcFloat   regs)	++ "\n"
	++ "     double: " ++ (show $ getFreeRegs RcDouble  regs)	++ "\n"
-}

{-
-- | Check whether a reg is free
regIsFree :: RegNo -> FreeRegs -> Bool
regIsFree r (FreeRegs g f d)

	-- a general purpose reg
	| r <= 31	
	, mask	<- 1 `shiftL` fromIntegral r
	= g .&. mask /= 0

	-- use the first 22 float regs as double precision
	| r >= 32
	, r <= 53
	, mask	<- 1 `shiftL` (fromIntegral r - 32)
	= d .&. mask /= 0

	-- use the last 10 float regs as single precision
	| otherwise 
	, mask	<- 1 `shiftL` (fromIntegral r - 32)
	= f .&. mask /= 0
-}

-- | Grab a register.
grabReg :: RegNo -> FreeRegs -> FreeRegs
grabReg r (FreeRegs g f d)

	-- a general purpose reg
	| r <= 31
	, mask	<- complement (1 `shiftL` fromIntegral r)
	= FreeRegs (g .&. mask) f d
    
	-- use the first 22 float regs as double precision
	| r >= 32
	, r <= 53
	, mask	<- complement (1 `shiftL` (fromIntegral r - 32))
	= FreeRegs g f (d .&. mask)

	-- use the last 10 float regs as single precision
	| otherwise
	, mask	<- complement (1 `shiftL` (fromIntegral r - 32))
	= FreeRegs g (f .&. mask) d



-- | Release a register from allocation.
--	The register liveness information says that most regs die after a C call, 
--	but we still don't want to allocate to some of them.
--
releaseReg :: RegNo -> FreeRegs -> FreeRegs
releaseReg r regs@(FreeRegs g f d)
	-- don't release pinned reg
	| not $ isFastTrue (freeReg r)
	= regs
	
	-- don't release the high part of double regs
	--	this prevents them from being allocated as single precison regs.
	| r == 39	= regs
	| r == 41	= regs
	| r == 43	= regs
	| r == 45	= regs
	| r == 47	= regs
	| r == 49	= regs
	| r == 51	= regs
	| r == 53	= regs
	
	-- a general purpose reg
	| r <= 31	
	, mask	<- 1 `shiftL` fromIntegral r
	= FreeRegs (g .|. mask) f d

	-- use the first 22 float regs as double precision
	| r >= 32
	, r <= 53
	, mask	<- 1 `shiftL` (fromIntegral r - 32)
	= FreeRegs g f (d .|. mask)

	-- use the last 10 float regs as single precision
	| otherwise 
	, mask	<- 1 `shiftL` (fromIntegral r - 32)
	= FreeRegs g (f .|. mask) d


-- | Allocate a register in the map.
allocateReg :: RegNo -> FreeRegs -> FreeRegs
allocateReg r regs -- (FreeRegs g f d) 

	-- if the reg isn't actually free then we're in trouble
{-	| not $ regIsFree r regs
	= pprPanic 
		"RegAllocLinear.allocateReg"
		(text "reg " <> ppr r <> text " is not free")
-}  
	| otherwise
	= grabReg r regs


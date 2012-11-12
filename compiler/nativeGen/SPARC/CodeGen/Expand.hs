
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | Expand out synthetic instructions into single machine instrs.
module SPARC.CodeGen.Expand (
	expandTop
)

where

import SPARC.Instr
import SPARC.Imm
import SPARC.AddrMode
import SPARC.Regs
import SPARC.Ppr	()
import Instruction
import Reg
import Size
import Cmm


import Outputable
import OrdList

-- | Expand out synthetic instructions in this top level thing
expandTop :: NatCmmDecl CmmStatics Instr -> NatCmmDecl CmmStatics Instr
expandTop top@(CmmData{})
	= top

expandTop (CmmProc info lbl live (ListGraph blocks))
	= CmmProc info lbl live (ListGraph $ map expandBlock blocks)


-- | Expand out synthetic instructions in this block
expandBlock :: NatBasicBlock Instr -> NatBasicBlock Instr

expandBlock (BasicBlock label instrs)
 = let	instrs_ol	= expandBlockInstrs instrs
 	instrs'		= fromOL instrs_ol
   in	BasicBlock label instrs'


-- | Expand out some instructions
expandBlockInstrs :: [Instr] -> OrdList Instr
expandBlockInstrs []	= nilOL
	
expandBlockInstrs (ii:is)
 = let	ii_doubleRegs	= remapRegPair ii
	is_misaligned	= expandMisalignedDoubles ii_doubleRegs

   in	is_misaligned `appOL` expandBlockInstrs is
   


-- | In the SPARC instruction set the FP register pairs that are used
--	to hold 64 bit floats are refered to by just the first reg 
--	of the pair. Remap our internal reg pairs to the appropriate reg.
--
--	For example:
--	    ldd [%l1], (%f0 | %f1)
--
--	gets mapped to
--	    ldd [$l1], %f0
--
remapRegPair :: Instr -> Instr
remapRegPair instr
 = let	patchF reg
 	 = case reg of
	 	RegReal (RealRegSingle _) 	
			-> reg

		RegReal (RealRegPair r1 r2)	

			-- sanity checking
		 	| r1 	     >= 32
			, r1 	     <= 63
			, r1 `mod` 2 == 0	
			, r2         == r1 + 1	
			-> RegReal (RealRegSingle r1)

			| otherwise		
			-> pprPanic "SPARC.CodeGen.Expand: not remapping dodgy looking reg pair " (ppr reg)

		RegVirtual _
			-> pprPanic "SPARC.CodeGen.Expand: not remapping virtual reg " (ppr reg)
			
   in	patchRegsOfInstr instr patchF




-- Expand out 64 bit load/stores into individual instructions to handle
--	possible double alignment problems.
--
--	TODO: 	It'd be better to use a scratch reg instead of the add/sub thing.
--		We might be able to do this faster if we use the UA2007 instr set
--		instead of restricting ourselves to SPARC V9.
--
expandMisalignedDoubles :: Instr -> OrdList Instr
expandMisalignedDoubles instr

	-- Translate to:
	--    add g1,g2,g1
	--    ld  [g1],%fn
	--    ld  [g1+4],%f(n+1)
	--    sub g1,g2,g1           -- to restore g1
 	| LD FF64 (AddrRegReg r1 r2) fReg	<- instr
	= 	toOL	[ ADD False False r1 (RIReg r2) r1
			, LD  FF32  (AddrRegReg r1 g0) 		fReg
			, LD  FF32  (AddrRegImm r1 (ImmInt 4))	(fRegHi fReg)
			, SUB False False r1 (RIReg r2) r1 ]

	-- Translate to
	--    ld  [addr],%fn
	--    ld  [addr+4],%f(n+1)
	| LD FF64 addr fReg			<- instr
	= let	Just addr'	= addrOffset addr 4
	  in	toOL	[ LD  FF32  addr	fReg
	  		, LD  FF32  addr'	(fRegHi fReg) ]

	-- Translate to:
	--    add g1,g2,g1
	--    st  %fn,[g1]
	--    st  %f(n+1),[g1+4]
	--    sub g1,g2,g1           -- to restore g1
	| ST FF64 fReg (AddrRegReg r1 r2)	<- instr
	= 	toOL	[ ADD False False r1 (RIReg r2) r1
			, ST  FF32  fReg           (AddrRegReg r1 g0) 		
			, ST  FF32  (fRegHi fReg)  (AddrRegImm r1 (ImmInt 4))	
			, SUB False False r1 (RIReg r2) r1 ]

	-- Translate to
	--    ld  [addr],%fn
	--    ld  [addr+4],%f(n+1)
	| ST FF64 fReg addr			<- instr
	= let	Just addr'	= addrOffset addr 4
	  in	toOL	[ ST  FF32  fReg           addr	
	  		, ST  FF32  (fRegHi fReg)  addr'	 ]

	-- some other instr
	| otherwise
	= unitOL instr



-- | The the high partner for this float reg.	
fRegHi :: Reg -> Reg
fRegHi (RegReal (RealRegSingle r1))
	| r1		>= 32
	, r1		<= 63
	, r1 `mod` 2 == 0
	= (RegReal $ RealRegSingle (r1 + 1))
	
-- Can't take high partner for non-low reg.
fRegHi reg
	= pprPanic "SPARC.CodeGen.Expand: can't take fRegHi from " (ppr reg)
	
	
	
	
	



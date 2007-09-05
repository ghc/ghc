-- | Register coalescing.
--

module RegCoalesce (
	regCoalesce,
	slurpJoinMovs
)

where

import Cmm
import MachRegs
import RegLiveness
import RegAllocInfo

import Bag
import UniqFM
import UniqSet
import UniqSupply

import Control.Monad
import Data.List

-- | Do register coalescing on this top level thing
--	For Reg -> Reg moves, if the first reg dies at the same time the second reg is born
--	then the mov only serves to join live ranges. The two regs can be renamed to be 
--	the same and the move instruction safely erased.

regCoalesce :: [LiveCmmTop] -> UniqSM [LiveCmmTop]
regCoalesce code
 = do	
 	let joins	= foldl' unionBags emptyBag
			$ map slurpJoinMovs code

	let alloc	= foldl' buildAlloc emptyUFM 
			$ bagToList joins

	let patched	= map (patchEraseLive (sinkReg alloc)) code
			
	return patched


buildAlloc :: UniqFM Reg -> (Reg, Reg) -> UniqFM Reg
buildAlloc fm (r1, r2)
 = let	rmin	= min r1 r2
 	rmax	= max r1 r2
   in	addToUFM fm rmax rmin

sinkReg :: UniqFM Reg -> Reg -> Reg
sinkReg fm r
 = case lookupUFM fm r of
 	Nothing	-> r
	Just r'	-> sinkReg fm r'	
	

-- | Slurp out mov instructions that only serve to join live ranges.
--	During a mov, if the source reg dies and the destiation reg is born
--	then we can rename the two regs to the same thing and eliminate the move.
--
slurpJoinMovs :: LiveCmmTop -> Bag (Reg, Reg)
slurpJoinMovs live
	= slurpCmm emptyBag live
 where	
  	slurpCmm   rs  CmmData{}		         = rs
	slurpCmm   rs (CmmProc _ _ _ (ListGraph blocks)) = foldl' slurpComp  rs blocks
   	slurpComp  rs (BasicBlock _ blocks)	         = foldl' slurpBlock rs blocks
        slurpBlock rs (BasicBlock _ instrs)              = foldl' slurpLI    rs instrs
                
        slurpLI    rs (Instr _	Nothing)	         = rs
	slurpLI    rs (Instr instr (Just live))
	 	| Just (r1, r2)	<- isRegRegMove instr
		, elementOfUniqSet r1 $ liveDieRead live
		, elementOfUniqSet r2 $ liveBorn live

		-- only coalesce movs between two virtuals for now, else we end up with
		--	allocatable regs in the live regs list.. 
		, isVirtualReg r1 && isVirtualReg r2
		= consBag (r1, r2) rs
		
		| otherwise
		= rs
	
	

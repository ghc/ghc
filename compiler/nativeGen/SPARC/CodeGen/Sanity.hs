
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | One ounce of sanity checking is worth 10000000000000000 ounces 
--	of staring blindly at assembly code trying to find the problem..
--
module SPARC.CodeGen.Sanity (
	checkBlock
)

where

import SPARC.Instr
import SPARC.Ppr	()
import Instruction

import Cmm

import Outputable


-- | Enforce intra-block invariants.
--
checkBlock :: CmmBlock
           -> NatBasicBlock Instr
           -> NatBasicBlock Instr

checkBlock cmm block@(BasicBlock _ instrs)
	| checkBlockInstrs instrs
	= block
	
	| otherwise
	= pprPanic 
		("SPARC.CodeGen: bad block\n")
		( vcat	[ text " -- cmm -----------------\n"
			, ppr cmm
			, text " -- native code ---------\n"
			, ppr block ])


checkBlockInstrs :: [Instr] -> Bool
checkBlockInstrs ii

	-- An unconditional jumps end the block.
	--	There must be an unconditional jump in the block, otherwise
	--	the register liveness determinator will get the liveness
	--	information wrong. 
	--
	--	If the block ends with a cmm call that never returns
	--	then there can be unreachable instructions after the jump,
	--	but we don't mind here.
	--
	| instr : NOP : _	<- ii 
	, isUnconditionalJump instr
	= True
	
	-- All jumps must have a NOP in their branch delay slot.
	--	The liveness determinator and register allocators aren't smart
	--	enough to handle branch delay slots.
	--
	| instr : NOP : is	<- ii
	, isJumpishInstr instr
	= checkBlockInstrs is

	-- keep checking
	| _:i2:is		<- ii
	= checkBlockInstrs (i2:is)

	-- this block is no good	
	| otherwise
	= False



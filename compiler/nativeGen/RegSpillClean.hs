-- | Clean out unneeded spill/reload instrs
--
module RegSpillClean (
	cleanSpills
)
where

import RegLiveness
import RegAllocInfo
import MachRegs
import MachInstrs
import Cmm

import UniqSet


-- | Clean out unneeded spill/reloads from this top level thing.
cleanSpills :: LiveCmmTop -> LiveCmmTop
cleanSpills cmm
	= mapBlockTop cleanBlock cmm
 where
	cleanBlock (BasicBlock id instrs)
		= BasicBlock id
		$ cleanSpill  emptyUniqSet []
		$ cleanReload emptyUniqSet []
		$ instrs


-- | Clean out unneeded reload instructions.
--	Walking forwards across the code
--	  If there are no writes to a reg between a reload and the
--	  last spill or reload then we don't need the reload.
--
cleanReload
	:: UniqSet Reg 		-- ^ hregs that were reloaded but not written to yet
	-> [LiveInstr]		-- ^ acc
	-> [LiveInstr] 		-- ^ instrs to clean (in backwards order)
	-> [LiveInstr]		-- ^ cleaned instrs  (in forward   order)

cleanReload valid acc []	= acc
cleanReload valid acc (li@(Instr instr live) : instrs)
	| SPILL reg slot	<- instr
	, valid'		<- addOneToUniqSet valid reg
	= cleanReload valid' (li : acc) instrs

	| RELOAD slot reg	<- instr
	= if elementOfUniqSet reg valid
	   then	cleanReload valid acc instrs
	   else cleanReload (addOneToUniqSet valid reg) (li : acc) instrs

	| RU read written	<- regUsage instr
	, valid'		<- minusUniqSet valid (mkUniqSet written)
	= cleanReload valid' (li : acc) instrs


-- | Clean out unneeded spill instructions.
--	Walking backwards across the code.
--	 If there were no reloads from a slot between a spill and the last one
--	 then the slot was never read and we don't need the spill.

cleanSpill
	:: UniqSet Int 		-- ^ slots that have been spilled, but not reload from
	-> [LiveInstr]		-- ^ acc
	-> [LiveInstr]		-- ^ instrs to clean (in forwards order)
	-> [LiveInstr]		-- ^ cleaned instrs  (in backwards order)

cleanSpill unused acc []	= acc
cleanSpill unused acc (li@(Instr instr live) : instrs)
	| SPILL reg slot	<- instr
	= if elementOfUniqSet slot unused
	   then cleanSpill unused acc instrs
	   else cleanSpill (addOneToUniqSet unused slot) (li : acc) instrs

	| RELOAD slot reg	<- instr
	, unused'		<- delOneFromUniqSet unused slot
	= cleanSpill unused' (li : acc) instrs

	| otherwise
	= cleanSpill unused (li : acc) instrs


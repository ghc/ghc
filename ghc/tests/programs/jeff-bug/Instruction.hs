module Instruction where

import Arithmetic
import Memory

-- Begin Signature ------------------------------------------------------

{- 

While not knowing the details of a particular instruction set, the
Instruction class allows code defined in arithmetic and other
modules to do the right thing for the often-defined instructions.

-}


class (Show i, Eq i) => Instruction i where
   
   -- is a nop instruction?
   isNoOp :: i -> Bool

   -- is an add instruction?
   isAddOp :: i -> Bool
   isSubOp :: i -> Bool
   isMultOp :: i -> Bool
   isDivOp :: i -> Bool
   isJumpOp :: i -> Bool
   isMemOp :: i -> Bool
   isLoadOp :: i -> Bool
   isStoreOp :: i -> Bool
   isAluOp  :: i -> Bool
   isCmpOp  :: i -> Bool
   isBoolOp  :: i -> Bool
   isMoveOp :: i -> Bool

   -- map the instruction to an AluOp (undefined if not isAluOp)
   aluOp :: i -> AluOp 

   -- is a conditional instruction?
   isCond :: i -> Bool

   -- is a parallel instruction? 
   -- example:  [r1,r2] <- SWAP [r1,r2] can be mapped two instruction
   isPar :: i -> Bool
   -- get the first instruction (if isPar)
   fstOp :: i -> AluOp
   -- get the second instruction (if isPar)
   sndOp :: i -> AluOp

   memOp :: i -> LoadStoreOp

   noOp :: i


-- End Signature ------------------------------------------------------









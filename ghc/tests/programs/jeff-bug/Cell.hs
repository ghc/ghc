module Cell where

import Register
import Words

-- Begin Signature: Cell ----------------------------------------------
{-

Cells are intended to be used to represent the source and destination
operands in machine instructions.  Consider, for example:
	
	r1=? <- r20=15 + 8

Here the first cell (r1=?) is a register reference, and its value is
not known yet.  The source cell r20=15 is a register reference with 
its value calculated.  8 is the other source operand --- in this 
case a constant.  The Cell class hopes to capture this notion, while
allowing you freedom to define richer Cell-like structures.

The Cell's interface supports register references, constants,
PCs, speculative PCs and predicates.

See the DLX_Cell module for a concrete instance

Currently several of the Cell methods overlap with each other ---- 
Eventually we will slim the methods to the minimum set.
-}


class Cell c where

  -- return a PC register reference with no value
  pcNothing	:: (Register r,Word w) => c r w

  -- return the value within the cell (undefined if no value exists)
  getVal	:: (Register r,Word w) => c r w -> w

  -- update the value within the cell
  putVal	:: (Register r,Word w) => c r w -> Maybe w -> c r w

  -- place the cell in an invalid state 
  invalidate   	:: (Register r,Word w) => c r w -> c r w

  -- is the cell a register reference?
  isReg		:: (Register r,Word w) => c r w -> Bool

  -- is the cell a PC register reference?
  isPC		:: (Register r,Word w) => c r w -> Bool

  -- is the cell a speculative PC register reference?
  isSpecPC	:: (Register r,Word w) => c r w -> Bool

  -- is the cell indicates a location in memory?
  isLoc		:: (Register r,Word w) => c r w -> Bool

  -- is the cell  a constant value?
  isVal		:: (Register r,Word w) => c r w -> Bool

  -- is the cell in an invalide state?
  isInv		:: (Register r,Word w) => c r w -> Bool

  -- is the value of the cell is known?
  --    isVal (r2=6) = True
  --    isVal (r2=?) = False
  isAss		:: (Register r,Word w) => c r w -> Bool

  -- is the cell a predicate register reference?
  isPred	:: (Register r,Word w) => c r w -> Bool

  -- has the value been calculated?  (ie. isAss || isInv)
  isComputed	:: (Register r,Word w) => c r w -> Bool

  -- are the two cells both refering to the same register?
  sameLoc	:: (Register r,Word w) => c r w -> c r w -> Bool

  -- true if sameLoc is true and neither cell is invalid
  cellHazard	:: (Register r,Word w) => c r w -> c r w -> Bool

  -- get the register reference
  getReg        :: (Register r,Word w) => c r w -> r

  -- construct a cell with a memory reference
  loc           :: Word w => w -> c r w

  isPred x = False

-- End Signature: Cell ----------------------------------------------





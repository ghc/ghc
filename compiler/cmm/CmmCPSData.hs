module CmmCPSData (
  blocksToBlockEnv,
  BrokenBlock(..),
  BlockEntryInfo(..),
  FinalStmt(..)
  ) where

#include "HsVersions.h"

import Cmm
import CLabel

import UniqFM

-- A minor helper (TODO document)
blocksToBlockEnv :: [BrokenBlock] -> BlockEnv BrokenBlock
blocksToBlockEnv blocks = listToUFM $ map (\b -> (brokenBlockId b, b)) blocks

data BrokenBlock
  = BrokenBlock {
      brokenBlockId :: BlockId, -- Like a CmmBasicBlock
      brokenBlockEntry :: BlockEntryInfo,
                                -- How this block can be entered

      brokenBlockStmts :: [CmmStmt],
                                -- Like a CmmBasicBlock
                                -- (but without the last statement)

      brokenBlockTargets :: [BlockId],
                                -- Blocks that this block could
                                -- branch to one either by conditional
                                -- branches or via the last statement

      brokenBlockExit :: FinalStmt
                                -- How the block can be left
    }

data BlockEntryInfo
  = FunctionEntry		-- Beginning of a function
      CLabel                    -- The function name
      CmmFormals                -- Aguments to function

  | ContinuationEntry 		-- Return point of a call
      CmmFormals                -- return values (argument to continuation)
  -- TODO:
  -- | ProcPointEntry -- no return values, but some live might end up as params or possibly in the frame

  | ControlEntry		-- A label in the input

-- Final statement in a BlokenBlock
-- Constructors and arguments match those in Cmm,
-- but are restricted to branches, returns, jumps, calls and switches
data FinalStmt
  = FinalBranch
      BlockId -- next block (must be a ControlEntry)

  | FinalReturn
      CmmActuals -- return values

  | FinalJump
      CmmExpr -- the function to call
      CmmActuals -- arguments to call

  | FinalCall
      BlockId -- next block after call (must be a ContinuationEntry)
      CmmCallTarget -- the function to call
      CmmFormals -- results from call (redundant with ContinuationEntry)
      CmmActuals -- arguments to call
      (Maybe [GlobalReg]) -- registers that must be saved (TODO)

  | FinalSwitch
      CmmExpr [Maybe BlockId]   -- Table branch

  -- TODO: | ProcPointExit (needed?)

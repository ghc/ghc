module CmmBrokenBlock (
  BrokenBlock(..),
  BlockEntryInfo(..),
  FinalStmt(..),
  breakBlock,
  cmmBlockFromBrokenBlock,
  blocksToBlockEnv,
  ) where

#include "HsVersions.h"

import Cmm
import CLabel

import Maybes
import Panic
import Unique
import UniqFM

-----------------------------------------------------------------------------
-- Data structures
-----------------------------------------------------------------------------

-- |Similar to a 'CmmBlock' with a little extra information
-- to help the CPS analysis.
data BrokenBlock
  = BrokenBlock {
      brokenBlockId :: BlockId, -- ^ The block's label like a 'CmmBasicBlock'
      brokenBlockEntry :: BlockEntryInfo,
                                -- ^ Ways this block can be entered

      brokenBlockStmts :: [CmmStmt],
                                -- ^ Body like a CmmBasicBlock
                                -- (but without the last statement)

      brokenBlockTargets :: [BlockId],
                                -- ^ Blocks that this block could
                                -- branch to one either by conditional
                                -- branches or via the last statement

      brokenBlockExit :: FinalStmt
                                -- ^ The final statement of the block
    }

-- | How a block could be entered
data BlockEntryInfo
  = FunctionEntry		-- ^ Block is the beginning of a function
      CLabel                    -- ^ The function name
      CmmFormals                -- ^ Aguments to function

  | ContinuationEntry 		-- ^ Return point of a function call
      CmmFormals                -- ^ return values (argument to continuation)

  | ControlEntry		-- ^ Any other kind of block.
                                -- Only entered due to control flow.

  -- TODO: Consider adding ProcPointEntry
  -- no return values, but some live might end up as
  -- params or possibly in the frame


-- | Final statement in a 'BlokenBlock'.
-- Constructors and arguments match those in 'Cmm',
-- but are restricted to branches, returns, jumps, calls and switches
data FinalStmt
  = FinalBranch                 -- ^ Same as 'CmmBranch'
      BlockId                   -- ^ Target must be a ControlEntry

  | FinalReturn                 -- ^ Same as 'CmmReturn'
      CmmActuals                -- ^ Return values

  | FinalJump                   -- ^ Same as 'CmmJump'
      CmmExpr                   -- ^ The function to call
      CmmActuals                -- ^ Arguments of the call

  | FinalCall                   -- ^ Same as 'CmmForeignCall'
                                -- followed by 'CmmGoto'
      BlockId                   -- ^ Target of the 'CmmGoto'
                                -- (must be a 'ContinuationEntry')
      CmmCallTarget             -- ^ The function to call
      CmmHintFormals                -- ^ Results from call
                                -- (redundant with ContinuationEntry)
      CmmActuals                -- ^ Arguments to call

  | FinalSwitch                 -- ^ Same as a 'CmmSwitch'
      CmmExpr                   -- ^ Scrutinee (zero based)
      [Maybe BlockId]           -- ^ Targets

-----------------------------------------------------------------------------
-- Operations for broken blocks
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Takes a 'CmmBasicBlock' and breaks it up into a list of 'BrokenBlock'
-- by splitting on each 'CmmCall' in the 'CmmBasicBlock'.

breakBlock ::
    [Unique]                    -- ^ An infinite list of uniques
                                -- to create names of the new blocks with
    -> CmmBasicBlock            -- ^ Input block to break apart
    -> BlockEntryInfo           -- ^ Info for the first created 'BrokenBlock'
    -> [BrokenBlock]
breakBlock uniques (BasicBlock ident stmts) entry =
    breakBlock' uniques ident entry [] [] stmts
    where
      breakBlock' uniques current_id entry exits accum_stmts stmts =
          case stmts of
            [] -> panic "block doesn't end in jump, goto, return or switch"
            [CmmJump target arguments] ->
                [BrokenBlock current_id entry accum_stmts
                             exits
                             (FinalJump target arguments)]
            [CmmReturn arguments] ->
                [BrokenBlock current_id entry accum_stmts
                             exits
                             (FinalReturn arguments)]
            [CmmBranch target] ->
                [BrokenBlock current_id entry accum_stmts
                             (target:exits)
                             (FinalBranch target)]
            [CmmSwitch expr targets] ->
                [BrokenBlock current_id entry accum_stmts
                             (mapMaybe id targets ++ exits)
                             (FinalSwitch expr targets)]
            (CmmJump _ _:_) -> panic "jump in middle of block"
            (CmmReturn _:_) -> panic "return in middle of block"
            (CmmBranch _:_) -> panic "branch in middle of block"
            (CmmSwitch _ _:_) -> panic "switch in middle of block"

            -- Detect this special case to remain an inverse of
            -- 'cmmBlockFromBrokenBlock'
            {- TODO: Interferes with proc point detection
            [CmmCall target results arguments,
             CmmBranch next_id] -> [block]
              where
                block = do_call current_id entry accum_stmts exits next_id
                                target results arguments
             -}
            (CmmCall target results arguments:stmts) -> block : rest
              where
                next_id = BlockId $ head uniques
                block = do_call current_id entry accum_stmts exits next_id
                                target results arguments
                rest = breakBlock' (tail uniques) next_id
                                   (ContinuationEntry (map fst results)) [] [] stmts
            (s:stmts) ->
                breakBlock' uniques current_id entry
                            (cond_branch_target s++exits)
                            (accum_stmts++[s])
                            stmts

      do_call current_id entry accum_stmts exits next_id
              target results arguments =
          BrokenBlock current_id entry accum_stmts (next_id:exits)
                      (FinalCall next_id target results arguments)

      cond_branch_target (CmmCondBranch _ target) = [target]
      cond_branch_target _ = []

-----------------------------------------------------------------------------
-- | Convert from a BrokenBlock back to an equivalent CmmBasicBlock
-- Needed by liveness analysis
cmmBlockFromBrokenBlock :: BrokenBlock -> CmmBasicBlock
cmmBlockFromBrokenBlock (BrokenBlock ident _ stmts _ exit) =
    BasicBlock ident (stmts++exit_stmt)
    where
      exit_stmt =
          case exit of
            FinalBranch target -> [CmmBranch target]
            FinalReturn arguments -> [CmmReturn arguments]
            FinalJump target arguments -> [CmmJump target arguments]
            FinalSwitch expr targets -> [CmmSwitch expr targets]
            FinalCall branch_target call_target results arguments ->
                [CmmCall call_target results arguments,
                 CmmBranch branch_target]

-----------------------------------------------------------------------------
-- | Build a mapping so we can lookup a 'BrokenBlock' by its 'BlockId'
blocksToBlockEnv :: [BrokenBlock] -> BlockEnv BrokenBlock
blocksToBlockEnv blocks = listToUFM $ map (\b -> (brokenBlockId b, b)) blocks

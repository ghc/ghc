
module CmmBrokenBlock (
  BrokenBlock(..),
  BlockEntryInfo(..),
  FinalStmt(..),
  breakBlock,
  cmmBlockFromBrokenBlock,
  blocksToBlockEnv,
  adaptBlockToFormat,
  selectContinuations,
  ContFormat,
  makeContinuationEntries
  ) where

#include "HsVersions.h"

import BlockId
import Cmm
import CmmUtils
import CLabel

import CgUtils (callerSaveVolatileRegs)
import ClosureInfo

import Maybes
import Data.List
import Panic
import Unique

-- This module takes a 'CmmBasicBlock' which might have 'CmmCall'
-- statements in it with 'CmmSafe' set and breaks it up at each such call.
-- It also collects information about the block for later use
-- by the CPS algorithm.

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
                                -- branch to either by conditional
                                -- branches or via the last statement

      brokenBlockExit :: FinalStmt
                                -- ^ The final statement of the block
    }

-- | How a block could be entered
-- See Note [An example of CPS conversion]
data BlockEntryInfo
  = FunctionEntry CmmInfo CLabel CmmFormals
      -- ^ Block is the beginning of a function, parameters are:
      --   1. Function header info
      --   2. The function name
      --   3. Aguments to function
      -- Only the formal parameters are live

  | ContinuationEntry CmmFormals C_SRT Bool
      -- ^ Return point of a function call, parameters are:
      --   1. return values (argument to continuation)
      --   2. SRT for the continuation's info table
      --   3. True <=> GC block so ignore stack size
      -- Live variables, other than
      -- the return values, are on the stack

  | ControlEntry
      -- ^ Any other kind of block.  Only entered due to control flow.

  -- TODO: Consider adding ProcPointEntry
  -- no return values, but some live might end up as
  -- params or possibly in the frame

{-	Note [An example of CPS conversion]

This is NR's and SLPJ's guess about how things might work;
it may not be consistent with the actual code (particularly
in the matter of what's in parameters and what's on the stack).

f(x,y) {
   if x>2 then goto L
   x = x+1
L: if x>1 then y = g(y)
        else x = x+1 ;
   return( x+y )
}
	BECOMES

f(x,y) {   // FunctionEntry
   if x>2 then goto L
   x = x+1
L: 	   // ControlEntry
   if x>1 then push x; push f1; jump g(y)
        else x=x+1; jump f2(x, y)
}

f1(y) {    // ContinuationEntry
  pop x; jump f2(x, y);
}
  
f2(x, y) { // ProcPointEntry
  return (z+y);
}

-}

data ContFormat = ContFormat HintedCmmFormals C_SRT Bool
      -- ^ Arguments
      --   1. return values (argument to continuation)
      --   2. SRT for the continuation's info table
      --   3. True <=> GC block so ignore stack size
  deriving (Eq)

-- | Final statement in a 'BlokenBlock'.
-- Constructors and arguments match those in 'Cmm',
-- but are restricted to branches, returns, jumps, calls and switches
data FinalStmt
  = FinalBranch BlockId
    -- ^ Same as 'CmmBranch'.  Target must be a ControlEntry

  | FinalReturn HintedCmmActuals
    -- ^ Same as 'CmmReturn'. Parameter is the return values.

  | FinalJump CmmExpr HintedCmmActuals
    -- ^ Same as 'CmmJump'.  Parameters:
    --   1. The function to call,
    --   2. Arguments of the call

  | FinalCall BlockId CmmCallTarget HintedCmmFormals HintedCmmActuals
              C_SRT   CmmReturnInfo Bool
      -- ^ Same as 'CmmCallee' followed by 'CmmGoto'.  Parameters:
      --   1. Target of the 'CmmGoto' (must be a 'ContinuationEntry')
      --   2. The function to call
      --   3. Results from call (redundant with ContinuationEntry)
      --   4. Arguments to call
      --   5. SRT for the continuation's info table
      --   6. Does the function return?
      --   7. True <=> GC block so ignore stack size

  | FinalSwitch CmmExpr [Maybe BlockId]
      -- ^ Same as a 'CmmSwitch'.  Paremeters:
      --   1. Scrutinee (zero based)
      --   2. Targets

-----------------------------------------------------------------------------
-- Operations for broken blocks
-----------------------------------------------------------------------------

-- Naively breaking at *every* CmmCall leads to sub-optimal code.
-- In particular, a CmmCall followed by a CmmBranch would result
-- in a continuation that has the single CmmBranch statement in it.
-- It would be better have the CmmCall directly return to the block
-- that the branch jumps to.
--
-- This requires the target of the branch to look like the parameter
-- format that the CmmCall is expecting.  If other CmmCall/CmmBranch
-- sequences go to the same place they might not be expecting the
-- same format.  So this transformation uses the following solution.
-- First the blocks are broken up but none of the blocks are marked
-- as continuations yet.  This is the 'breakBlock' function.
-- Second, the blocks "vote" on what other blocks need to be continuations
-- and how they should be layed out.  Plurality wins, but other selection
-- methods could be selected at a later time.
-- This is the 'selectContinuations' function.
-- Finally, the blocks are upgraded to 'ContEntry' continuations
-- based on the results with the 'makeContinuationEntries' function,
-- and the blocks that didn't get the format they wanted for their
-- targets get a small adaptor block created for them by
-- the 'adaptBlockToFormat' function.
-- could be 

{-
UNUSED: 2008-12-29

breakProc ::
    [BlockId]                   -- ^ Any GC blocks that should be special
    -> [[Unique]]               -- ^ An infinite list of uniques
                                -- to create names of the new blocks with
    -> CmmInfo                  -- ^ Info table for the procedure
    -> CLabel                   -- ^ Name of the procedure
    -> CmmFormals               -- ^ Parameters of the procedure
    -> [CmmBasicBlock]          -- ^ Blocks of the procecure
                                -- (First block is the entry block)
    -> [BrokenBlock]

breakProc gc_block_idents uniques info ident params blocks =
    let
        (adaptor_uniques : block_uniques) = uniques

        broken_blocks :: ([(BlockId, ContFormat)], [BrokenBlock])
        broken_blocks =
            let new_blocks =
                    zipWith3 (breakBlock gc_block_idents)
                             block_uniques
                             blocks
                             (FunctionEntry info ident params :
                              repeat ControlEntry)
            in (concatMap fst new_blocks, concatMap snd new_blocks)

        selected = selectContinuations (fst broken_blocks)

    in map (makeContinuationEntries selected) $
       concat $
       zipWith (adaptBlockToFormat selected)
               adaptor_uniques
               (snd broken_blocks)
-}

-----------------------------------------------------------------------------
-- | Takes a 'CmmBasicBlock' and breaks it up into a list of 'BrokenBlock'
-- by splitting on each 'CmmCall' in the 'CmmBasicBlock'.

breakBlock ::
    [BlockId]                   -- ^ Any GC blocks that should be special
    -> [Unique]                 -- ^ An infinite list of uniques
                                -- to create names of the new blocks with
    -> CmmBasicBlock            -- ^ Input block to break apart
    -> BlockEntryInfo           -- ^ Info for the first created 'BrokenBlock'
    -> ([(BlockId, ContFormat)], [BrokenBlock])
breakBlock gc_block_idents uniques (BasicBlock ident stmts) entry =
    breakBlock' uniques ident entry [] [] stmts
    where
      breakBlock' uniques current_id entry exits accum_stmts stmts =
          case stmts of
            [] -> panic "block doesn't end in jump, goto, return or switch"

            -- Last statement.  Make the 'BrokenBlock'
            [CmmJump target arguments] ->
                ([],
                 [BrokenBlock current_id entry accum_stmts
                              exits
                              (FinalJump target arguments)])
            [CmmReturn arguments] ->
                ([],
                 [BrokenBlock current_id entry accum_stmts
                             exits
                             (FinalReturn arguments)])
            [CmmBranch target] ->
                ([],
                 [BrokenBlock current_id entry accum_stmts
                             (target:exits)
                             (FinalBranch target)])
            [CmmSwitch expr targets] ->
                ([],
                 [BrokenBlock current_id entry accum_stmts
                             (mapMaybe id targets ++ exits)
                             (FinalSwitch expr targets)])

            -- These shouldn't happen in the middle of a block.
            -- They would cause dead code.
            (CmmJump _ _:_) -> panic "jump in middle of block"
            (CmmReturn _:_) -> panic "return in middle of block"
            (CmmBranch _:_) -> panic "branch in middle of block"
            (CmmSwitch _ _:_) -> panic "switch in middle of block"

            -- Detect this special case to remain an inverse of
            -- 'cmmBlockFromBrokenBlock'
            [CmmCall target results arguments (CmmSafe srt) ret,
             CmmBranch next_id] ->
                ([cont_info], [block])
                where
                  cont_info = (next_id,
                               ContFormat results srt
                                              (ident `elem` gc_block_idents))
                  block = do_call current_id entry accum_stmts exits next_id
                                target results arguments srt ret

            -- Break the block on safe calls (the main job of this function)
            (CmmCall target results arguments (CmmSafe srt) ret : stmts) ->
                (cont_info : cont_infos, block : blocks)
                where
                  next_id = BlockId $ head uniques
                  block = do_call current_id entry accum_stmts exits next_id
                                  target results arguments srt ret

                  cont_info = (next_id,	-- Entry convention for the 
					-- continuation of the call
                               ContFormat results srt
                                              (ident `elem` gc_block_idents))

			-- Break up the part after the call
                  (cont_infos, blocks) = breakBlock' (tail uniques) next_id
                                         ControlEntry [] [] stmts

            -- Unsafe calls don't need a continuation
            -- but they do need to be expanded
            (CmmCall target results arguments CmmUnsafe ret : stmts) ->
                breakBlock' remaining_uniques current_id entry exits
                            (accum_stmts ++
                             arg_stmts ++
                             caller_save ++
                             [CmmCall target results new_args CmmUnsafe ret] ++
                             caller_load)
                            stmts
                where
                  (remaining_uniques, arg_stmts, new_args) =
                      loadArgsIntoTemps uniques arguments
                  (caller_save, caller_load) = callerSaveVolatileRegs (Just [])

            -- Default case.  Just keep accumulating statements
            -- and branch targets.
            (s : stmts) ->
                breakBlock' uniques current_id entry
                            (cond_branch_target s++exits)
                            (accum_stmts++[s])
                            stmts

      do_call current_id entry accum_stmts exits next_id
              target results arguments srt ret =
          BrokenBlock current_id entry accum_stmts (next_id:exits)
                      (FinalCall next_id target results arguments srt ret
                                     (current_id `elem` gc_block_idents))

      cond_branch_target (CmmCondBranch _ target) = [target]
      cond_branch_target _ = []

-----------------------------------------------------------------------------

selectContinuations :: [(BlockId, ContFormat)] -> [(BlockId, ContFormat)]
selectContinuations needed_continuations = formats
    where
      formats = map select_format format_groups
      format_groups = groupBy by_target needed_continuations
      by_target x y = fst x == fst y

      select_format formats = winner
          where
            winner = head $ head $ sortBy more_votes format_votes
            format_votes = groupBy by_format formats
            by_format x y = snd x == snd y
            more_votes x y = compare (length y) (length x)
              -- sort so the most votes goes *first*
              -- (thus the order of x and y is reversed)

makeContinuationEntries :: [(BlockId, ContFormat)]
                        -> BrokenBlock -> BrokenBlock
makeContinuationEntries formats
                        block@(BrokenBlock ident _entry stmts targets exit) =
    case lookup ident formats of
      Nothing -> block
      Just (ContFormat formals srt is_gc) ->
          BrokenBlock ident (ContinuationEntry (map hintlessCmm formals) srt is_gc)
                      stmts targets exit

adaptBlockToFormat :: [(BlockId, ContFormat)]
                   -> Unique
                   -> BrokenBlock
                   -> [BrokenBlock]
adaptBlockToFormat formats unique
                   block@(BrokenBlock ident entry stmts targets
                                      (FinalCall next target formals
                                                 actuals srt ret is_gc)) =
    if format_formals == formals &&
       format_srt == srt &&
       format_is_gc == is_gc
    then [block] -- Woohoo! This block got the continuation format it wanted
    else [adaptor_block, revised_block]
           -- This block didn't get the format it wanted for the
           -- continuation, so we have to build an adaptor.
    where
      (ContFormat format_formals format_srt format_is_gc) =
          maybe unknown_block id $ lookup next formats
      unknown_block = panic "unknown block in adaptBlockToFormat"

      revised_block = BrokenBlock ident entry stmts revised_targets revised_exit
      revised_targets = adaptor_ident : delete next targets
      revised_exit = FinalCall
                       adaptor_ident -- The only part that changed
                       target formals actuals srt ret is_gc

      adaptor_block = mk_adaptor_block adaptor_ident
                  (ContinuationEntry (map hintlessCmm formals) srt is_gc) next
      adaptor_ident = BlockId unique

      mk_adaptor_block :: BlockId -> BlockEntryInfo -> BlockId -> BrokenBlock
      mk_adaptor_block ident entry next =
          BrokenBlock ident entry [] [next] exit
              where
                exit = FinalJump
                         (CmmLit (CmmLabel (mkReturnPtLabel (getUnique next))))
                         (map formal_to_actual format_formals)

                formal_to_actual (CmmHinted reg hint)
                     = (CmmHinted (CmmReg (CmmLocal reg)) hint)
                -- TODO: Check if NoHint is right.  We're
                -- jumping to a C-- function not a foreign one
                -- so it might always be right.
adaptBlockToFormat _ _ block = [block]

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
            FinalCall branch_target call_target results arguments srt ret _ ->
                [CmmCall call_target results arguments (CmmSafe srt) ret,
                 CmmBranch branch_target]

-----------------------------------------------------------------------------
-- | Build a mapping so we can lookup a 'BrokenBlock' by its 'BlockId'
blocksToBlockEnv :: [BrokenBlock] -> BlockEnv BrokenBlock
blocksToBlockEnv blocks = mkBlockEnv $ map (\b -> (brokenBlockId b, b)) blocks

module CmmProcPoint (
  calculateProcPoints
  ) where

#include "HsVersions.h"

import BlockId
import CmmBrokenBlock
import Dataflow

import UniqSet
import Panic

-- Determine the proc points for a set of basic blocks.
--
-- A proc point is any basic block that must start a new function.
-- The entry block of the original function is a proc point.
-- The continuation of a function call is also a proc point.
-- The third kind of proc point arises when there is a joint point
-- in the control flow.  Suppose we have code like the following:
--
--   if (...) { ...; call foo(); ...}
--   else { ...; call bar(); ...}
--   x = y;
--
-- That last statement "x = y" must be a proc point because
-- it can be reached by blocks owned by different proc points
-- (the two branches of the conditional).
--
-- We calculate these proc points by starting with the minimal set
-- and finding blocks that are reachable from more proc points than
-- one of their parents.  (This ensures we don't choose a block
-- simply beause it is reachable from another block that is reachable
-- from multiple proc points.)  These new blocks are added to the
-- set of proc points and the process is repeated until there
-- are no more proc points to be found.

calculateProcPoints :: [BrokenBlock] -> UniqSet BlockId
calculateProcPoints blocks =
    calculateProcPoints' init_proc_points blocks
    where
      init_proc_points = mkUniqSet $
                         map brokenBlockId $
                         filter always_proc_point blocks
      always_proc_point BrokenBlock {
                              brokenBlockEntry = FunctionEntry _ _ _ } = True
      always_proc_point BrokenBlock {
                              brokenBlockEntry = ContinuationEntry _ _ _ } = True
      always_proc_point _ = False

calculateProcPoints' :: UniqSet BlockId -> [BrokenBlock] -> UniqSet BlockId
calculateProcPoints' old_proc_points blocks =
    if sizeUniqSet old_proc_points == sizeUniqSet new_proc_points
      then old_proc_points
      else calculateProcPoints' new_proc_points blocks
    where
      blocks_ufm :: BlockEnv BrokenBlock
      blocks_ufm = blocksToBlockEnv blocks

      owners = calculateOwnership blocks_ufm old_proc_points blocks
      new_proc_points =
          unionManyUniqSets
            (old_proc_points:
             map (calculateNewProcPoints owners) blocks)

calculateNewProcPoints :: BlockEnv (UniqSet BlockId)
                       -> BrokenBlock
                       -> UniqSet BlockId
calculateNewProcPoints  owners block =
    unionManyUniqSets (map (maybe_proc_point parent_id) child_ids)
    where
      parent_id = brokenBlockId block
      child_ids = brokenBlockTargets block
      maybe_proc_point parent_id child_id =
          if needs_proc_point
            then unitUniqSet child_id
            else emptyUniqSet
          where
            parent_owners = lookupWithDefaultBEnv owners emptyUniqSet parent_id
            child_owners = lookupWithDefaultBEnv owners emptyUniqSet child_id
            needs_proc_point =
                -- only if parent isn't dead
                (not $ isEmptyUniqSet parent_owners) &&
                -- and only if child has more owners than parent
                (not $ isEmptyUniqSet $
                     child_owners `minusUniqSet` parent_owners)

calculateOwnership :: BlockEnv BrokenBlock
                   -> UniqSet BlockId
                   -> [BrokenBlock]
                   -> BlockEnv (UniqSet BlockId)
calculateOwnership blocks_ufm proc_points blocks =
    fixedpoint dependants update (map brokenBlockId blocks) emptyBlockEnv
    where
      dependants :: BlockId -> [BlockId]
      dependants ident =
          brokenBlockTargets $ lookupWithDefaultBEnv
                                 blocks_ufm unknown_block ident

      update :: BlockId
             -> Maybe BlockId
             -> BlockEnv (UniqSet BlockId)
             -> Maybe (BlockEnv (UniqSet BlockId))
      update ident cause owners =
          case (cause, ident `elementOfUniqSet` proc_points) of
            (Nothing, True) ->
                Just $ extendBlockEnv owners ident (unitUniqSet ident)
            (Nothing, False) -> Nothing
            (Just _,      True) -> Nothing
            (Just cause', False) ->
                if (sizeUniqSet old) == (sizeUniqSet new)
                   then Nothing
                   else Just $ extendBlockEnv owners ident new
                where
                  old = lookupWithDefaultBEnv owners emptyUniqSet ident
                  new = old `unionUniqSets`
                        lookupWithDefaultBEnv owners emptyUniqSet cause'

      unknown_block = panic "unknown BlockId in calculateOwnership"

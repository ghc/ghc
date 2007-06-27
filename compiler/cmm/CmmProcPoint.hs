module CmmProcPoint (
  calculateProcPoints
  ) where

#include "HsVersions.h"

import Cmm
import CmmBrokenBlock
import Dataflow

import UniqSet
import UniqFM
import Panic

calculateOwnership :: BlockEnv BrokenBlock -> UniqSet BlockId -> [BrokenBlock] -> BlockEnv (UniqSet BlockId)
calculateOwnership blocks_ufm proc_points blocks =
    fixedpoint dependants update (map brokenBlockId blocks) emptyUFM
    where
      dependants :: BlockId -> [BlockId]
      dependants ident =
          brokenBlockTargets $ lookupWithDefaultUFM
                                 blocks_ufm unknown_block ident

      update :: BlockId -> Maybe BlockId
             -> BlockEnv (UniqSet BlockId) -> Maybe (BlockEnv (UniqSet BlockId))
      update ident cause owners =
          case (cause, ident `elementOfUniqSet` proc_points) of
            (Nothing, True) -> Just $ addToUFM owners ident (unitUniqSet ident)
            (Nothing, False) -> Nothing
            (Just cause', True) -> Nothing
            (Just cause', False) ->
                if (sizeUniqSet old) == (sizeUniqSet new)
                   then Nothing
                   else Just $ addToUFM owners ident new
                where
                  old = lookupWithDefaultUFM owners emptyUniqSet ident
                  new = old `unionUniqSets` lookupWithDefaultUFM owners emptyUniqSet cause'

      unknown_block = panic "unknown BlockId in selectStackFormat"

calculateProcPoints :: [BrokenBlock] -> UniqSet BlockId
calculateProcPoints blocks = calculateProcPoints' init_proc_points blocks
    where
      init_proc_points = mkUniqSet $
                         map brokenBlockId $
                         filter always_proc_point blocks
      always_proc_point BrokenBlock {
                              brokenBlockEntry = FunctionEntry _ _ } = True
      always_proc_point BrokenBlock {
                              brokenBlockEntry = ContinuationEntry _ _ } = True
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
      new_proc_points = unionManyUniqSets (old_proc_points:(map (calculateProcPoints'' owners) blocks))

calculateProcPoints'' :: BlockEnv (UniqSet BlockId) -> BrokenBlock -> UniqSet BlockId
calculateProcPoints''  owners block =
    unionManyUniqSets (map (f parent_id) child_ids)
    where
      parent_id = brokenBlockId block
      child_ids = brokenBlockTargets block
      -- TODO: name for f
      f parent_id child_id = 
          if needs_proc_point
            then unitUniqSet child_id
            else emptyUniqSet
          where
            parent_owners = lookupWithDefaultUFM owners emptyUniqSet parent_id
            child_owners = lookupWithDefaultUFM owners emptyUniqSet child_id
            needs_proc_point = not $ isEmptyUniqSet $ child_owners `minusUniqSet` parent_owners

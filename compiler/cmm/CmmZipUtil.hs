
module CmmZipUtil
  ( zipPreds
  , givesUniquePredecessorTo
  )
where
import BlockId
import Prelude hiding (last, unzip)
import ZipCfg

import Maybes
import UniqSet

-- | Compute the predecessors of each /reachable/ block
zipPreds :: LastNode l => LGraph m l -> BlockEnv BlockSet
zipPreds g = foldl add emptyBlockEnv (postorder_dfs g)
    where add env block@(Block id _) =
            foldl (\env sid ->
                       let preds = lookupBlockEnv env sid `orElse` emptyBlockSet
                       in  extendBlockEnv env sid (extendBlockSet preds id))
            env (succs block)

-- | Tell if a graph gives a block a unique predecessor.  For
-- efficiency, this function is designed to be partially applied.

givesUniquePredecessorTo :: LastNode l => LGraph m l -> BlockId -> Bool
givesUniquePredecessorTo g = \id -> elemBlockSet id singlePreds
    -- accumulates a pair of sets: the set of all blocks containing a single
    -- predecessor, and the set of all blocks containing at least two predecessors
    where (singlePreds, _) = fold_blocks add (emptyBlockSet, emptyBlockSet) g
          add b (single, multi) = foldl add_pred (single, multi) (succs b)
          add_pred pair@(single, multi) id =
              if elemBlockSet id multi then pair
              else if elemBlockSet id single then
                       (delOneFromUniqSet single id, extendBlockSet multi id)
                   else
                       (extendBlockSet single id, multi)
              
    


{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module CmmZipUtil 
  ( zipPreds
  )
where
import Prelude hiding (last, unzip)
import ZipCfg 
import Maybes

-- | Compute the predecessors of each *reachable* block
zipPreds :: LastNode l => LGraph m l -> BlockEnv BlockSet
zipPreds g = foldl add emptyBlockEnv (postorder_dfs g)
    where add env block@(Block id _) =
            foldl (\env sid ->
                       let preds = lookupBlockEnv env sid `orElse` emptyBlockSet
                       in  extendBlockEnv env sid (extendBlockSet preds id))
            env (succs block)

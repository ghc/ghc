module GHC.CmmToAsm.CFG.Weight
   ( Weights (..)
   , defaultWeights
   , parseWeights
   )
where

import GHC.Prelude
import GHC.Utils.Panic

-- | Edge weights to use when generating a CFG from CMM
data Weights = Weights
   { uncondWeight       :: Int
   , condBranchWeight   :: Int
   , switchWeight       :: Int
   , callWeight         :: Int
   , likelyCondWeight   :: Int
   , unlikelyCondWeight :: Int
   , infoTablePenalty   :: Int
   , backEdgeBonus      :: Int
   }

-- | Default edge weights
defaultWeights :: Weights
defaultWeights = Weights
   { uncondWeight       = 1000
   , condBranchWeight   = 800
   , switchWeight       = 1
   , callWeight         = -10
   , likelyCondWeight   = 900
   , unlikelyCondWeight = 300
   , infoTablePenalty   = 300
   , backEdgeBonus      = 400
   }

parseWeights :: String -> Weights -> Weights
parseWeights s oldWeights =
        foldl' (\cfg (n,v) -> update n v cfg) oldWeights assignments
    where
        assignments = map assignment $ settings s
        update "uncondWeight" n w =
            w {uncondWeight = n}
        update "condBranchWeight" n w =
            w {condBranchWeight = n}
        update "switchWeight" n w =
            w {switchWeight = n}
        update "callWeight" n w =
            w {callWeight = n}
        update "likelyCondWeight" n w =
            w {likelyCondWeight = n}
        update "unlikelyCondWeight" n w =
            w {unlikelyCondWeight = n}
        update "infoTablePenalty" n w =
            w {infoTablePenalty = n}
        update "backEdgeBonus" n w =
            w {backEdgeBonus = n}
        update other _ _
            = panic $ other ++
                      " is not a CFG weight parameter. " ++
                      exampleString
        settings s
            | (s1,rest) <- break (== ',') s
            , null rest
            = [s1]
            | (s1,rest) <- break (== ',') s
            = s1 : settings (drop 1 rest)

        assignment as
            | (name, _:val) <- break (== '=') as
            = (name,read val)
            | otherwise
            = panic $ "Invalid CFG weight parameters." ++ exampleString

        exampleString = "Example parameters: uncondWeight=1000," ++
            "condBranchWeight=800,switchWeight=0,callWeight=300" ++
            ",likelyCondWeight=900,unlikelyCondWeight=300" ++
            ",infoTablePenalty=300,backEdgeBonus=400"


{-# LANGUAGE ExperimentalDependentTypes, GADTs, RequiredTypeArguments, TypeApplications #-}
module PreludeFilterCutoff (CutoffBound(..), decideRow, packCutoff, showCutoffBound) where

import PreludeFilterLengthProof (AtMost)
import PreludeFilterRuntime (BooleanResult, decideBy, filterBoundBy)
import PreludeFilterSource (FilterWith)
import PreludeMapLengthSize (Size(..), size)
import PreludeMapLengthSource (Count)

-- | Compare two Peano sizes. Stop as soon as the result is known.
atMostSize :: Size -> Size -> Bool
atMostSize Zero _ = True
atMostSize (Succ _) Zero = False
atMostSize (Succ n) (Succ limit) = atMostSize n limit

-- | Check the row's size without inspecting its elements.
fits :: Size -> [a] -> Bool
fits cutoff row = atMostSize (size row) cutoff

-- | Use the actual local predicate as the index of its decision.
decideRow :: foreach (f :: [Bool] -> Bool)
          -> foreach (row :: [Bool]) -> BooleanResult (f row)
decideRow f row = decideBy @[Bool] f row

-- | Store the captured predicate, the input rows, and their filter size bound.
data CutoffBound where
    CutoffBound :: foreach (p :: [Bool] -> Bool) -> foreach (rows :: [[Bool]])
                -> AtMost (Count (FilterWith p rows)) (Count rows)
                -> CutoffBound

-- | Package the bound with the predicate that closes over the supplied cutoff.
packCutoff :: Size -> [[Bool]] -> CutoffBound
packCutoff cutoff rows =
    let predicate :: [Bool] -> Bool
        predicate row = fits cutoff row
        bound = filterBoundBy @[Bool] predicate rows
    in CutoffBound predicate rows bound

-- | Show the bound, the retained row sizes, and the original row count.
showCutoffBound :: CutoffBound -> String
showCutoffBound (CutoffBound predicate rows bound) =
    show (bound, map size (filter predicate rows), size rows)

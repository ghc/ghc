{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, RankNTypes #-}
module Opening (audit
#ifndef CHECK_INPUT_ESCAPE
               , viaFixedInput
#endif
               ) where

import Data.Maybe (fromMaybe)
import Filter
import Vector
import qualified WitnessSchema as W

anyCells :: Vec n a -> Vec m a -> [a]
anyCells Nil Nil = []
anyCells Nil (Cons a _) = [a]
anyCells (Cons a _) _ = [a]

audit :: Hidden Int -> Hidden Int -> [Int]
audit p q = anyCells p q

#ifdef CHECK_INPUT_ESCAPE
inputEscape :: W.RankedInput
inputEscape = body
  where
    body :: (forall r. ((forall a. a -> r) -> r) -> Maybe r) -> Int
    body receive = fromMaybe 19 (receive (\k -> k True))
#else
viaFixedInput :: W.RankedInput
viaFixedInput = body
  where
    body :: W.InputAt W.PolyId -> Int
    body receive = fromMaybe 43 (receive (fst . W.atTwoTypes))
#endif

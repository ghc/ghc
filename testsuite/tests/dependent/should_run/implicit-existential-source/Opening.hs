{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, RankNTypes #-}
module Opening (audit
#if !defined(CHECK_INPUT_ESCAPE) && !defined(CHECK_FORWARD_ESCAPE)
               , viaFixedInput, viaForwardedInput
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

#if defined(CHECK_INPUT_ESCAPE) || defined(CHECK_FORWARD_ESCAPE)
inputEscape :: W.RankedInput
#ifdef CHECK_FORWARD_ESCAPE
inputEscape = \receive -> body receive
#else
inputEscape = body
#endif
  where
    body :: (forall r. ((forall a. a -> r) -> r) -> Maybe r) -> Int
    body receive = fromMaybe 19 (receive (\k -> k True))
#else
viaFixedInput :: W.RankedInput
viaFixedInput = body
  where
    body :: W.InputAt W.PolyId -> Int
    body receive = fromMaybe 43 (receive (fst . W.atTwoTypes))

viaForwardedInput :: W.RankedInput
viaForwardedInput = \receive -> body receive
  where
    body :: W.InputAt W.PolyId -> Int
    body receive = fromMaybe 47 (receive (fst . W.atTwoTypes))
#endif

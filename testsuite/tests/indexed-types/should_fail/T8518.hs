{-# LANGUAGE TypeFamilies #-}
module T8518 where

import Data.Maybe
import Control.Applicative

class Continuation c where
    type Z c
    type B c
    type F c
    continue ::  c -> (Z c) -> (B c) -> Maybe ((F c), c)

callCont :: Continuation c => c -> (Z c) -> (B c) -> Maybe (F c)
callCont c z b = rpt (4 :: Int) c z b
    where
        rpt 0 c' z' b' = fromJust (fst <$> (continue c' z' b'))
        rpt i c' z' b' = let c'' = fromJust (snd <$> (continue c' z' b')) in rpt (i-1) c''
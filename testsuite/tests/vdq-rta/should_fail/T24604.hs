module T24604 where

import Data.Kind

type UF :: forall zk -> zk -> Constraint
class UF kk (xb :: k) where
    op :: (xs::kk) -> Bool

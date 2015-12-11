{-# LANGUAGE TypeInType, RankNTypes #-}

module T11142 where

import Data.Kind

data SameKind :: k -> k -> *

foo :: forall b. (forall k (a :: k). SameKind a b) -> ()
foo = undefined

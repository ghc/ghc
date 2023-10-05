{-# LANGUAGE TypeAbstractions, GADTs #-}

module T19847b where

import Data.Kind

data T (a :: Type) b where
  MkT4 :: forall a b. b ~ a => T a b

foo x = (case x of MkT4 @Bool ->  ()) :: ()

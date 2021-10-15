{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}

module LevPolyLet
  ( example
  ) where

import GHC.Exts

-- This should be rejected because of the let binding.
example :: forall (v :: Levity) (a :: TYPE ('BoxedRep v)).
     (Int -> a)
  -> Bool
example f =
  let x = f 42
   in undefined

{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}

module LevPolyResult (example) where

import GHC.Exts

example :: forall (v :: Levity) (a :: TYPE ('BoxedRep v)). (Int -> a) -> a
example f = f 42

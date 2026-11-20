{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TemplateHaskell #-}
module NativeTypeCaseRetainedReify where

import Control.Monad (unless)
import Language.Haskell.TH (recover, reify, reifyType)

type Select (b :: Bool) = case b of { True -> Int; False -> Char }

data Named where
  Named :: (foreach (b :: Bool) -> Select b) -> Named

data Inline where
  Inline :: (foreach (b :: Bool) -> (case b of { True -> Int; False -> Char })) -> Inline

data Stored where
  Stored :: foreach (b :: Bool) -> Select b -> Stored

value :: foreach (b :: Bool) -> Select b
value True = 11
value False = 'x'

$(do named <- recover (pure True) (False <$ reifyType 'Named)
     stored <- recover (pure True) (False <$ reify ''Stored)
     signature <- recover (pure True) (False <$ reifyType 'value)
     unless (named && stored && signature) (fail "Expected reification to reject retained types")
     [] <$ reifyType 'Inline)

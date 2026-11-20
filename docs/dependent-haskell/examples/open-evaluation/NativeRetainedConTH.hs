{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TemplateHaskell #-}
-- | Store a value whose type depends on a preceding runtime argument.
module NativeRetainedConTH (Stored(..), renderStored) where

import NativeRetainedTH (Select)

$([d|
  data Stored where
    Stored :: foreach (b :: Bool) -> Select b -> Stored
  |])

-- | Read the stored index and render the payload selected by it.
renderStored :: Stored -> String
renderStored (Stored True n) = show n
renderStored (Stored False c) = [c]

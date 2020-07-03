{-# LANGUAGE UnliftedNewtypes, StandaloneKindSignatures, TypeFamilies, GADTs, DataKinds #-}

module T18357b where

import Data.Kind

type family Star where Star = Type

newtype T :: Type where
  MkT :: Int -> (T :: Star)

-- The error message is pretty terrible
-- but it probably nevery happens in practice

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnliftedDatatypes #-}

module T25374A where

import GHC.Exts

type NP :: [UnliftedType] -> UnliftedType
data NP xs where
  UNil :: NP '[]
  (::*) :: x -> NP xs -> NP (x ': xs)


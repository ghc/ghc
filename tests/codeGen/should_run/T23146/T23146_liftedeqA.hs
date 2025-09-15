{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnliftedDatatypes #-}

module T23146_liftedeqA where

import GHC.Exts

type NP :: [UnliftedType] -> UnliftedType
data NP xs where
  UNil :: NP '[]
  (::*) :: x -> NP xs -> NP (x ': xs)


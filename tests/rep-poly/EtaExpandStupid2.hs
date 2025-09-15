
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module EtaExpandStupid2 where

import Data.Kind
import GHC.Exts

type D4 :: TYPE FloatRep -> Type -> Type -> Type
data (Eq b, Num c) => D4 a b c = MkD4 a b c

foo4 :: Int -> c -> D4 Float# Int c
foo4 = MkD4 ( 9.0# `timesFloat#` 17.0# )

  -- should fail: no evidence for Num c,
  -- which is required by the datatype context

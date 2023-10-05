{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module RepPolyInferPatSyn where

import Data.Kind
import GHC.Exts

type R :: RuntimeRep
type family R where {}

type T :: TYPE R
type family T where {}

pattern P a = (a :: T)

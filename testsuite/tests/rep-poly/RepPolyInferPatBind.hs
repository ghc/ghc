{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module RepPolyInferPatBind where

import Data.Kind
import GHC.Exts

type R :: RuntimeRep
type family R where {}

type T :: TYPE R
type family T where {}

(x :: T) = x

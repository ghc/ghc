{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}

module RepPolyPatBind where

import Data.Kind
import GHC.Exts

foo :: forall rep (a :: TYPE rep). () -> a
foo _ =
  let
    x, y :: a
    (# x, y #) = undefined
  in x

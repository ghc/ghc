{-# LANGUAGE DataKinds #-}
module M where

import GHC.Exts
import Data.Kind

f :: forall f (g :: Type) (a :: TYPE (f g)). Int -> a
f = f

x = f 0

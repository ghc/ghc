{-# OPTIONS_GHC -Werror=name-shadowing #-}

{-# LANGUAGE TypeAbstractions #-}

module T22478d where

import Data.Kind (Type)

data T (a :: Type) = MkT

fShadowing1 (MkT @a) = g (MkT @a)
  where
    g (MkT @a) = ()

fShadowing2 :: T a -> T b -> T b
fShadowing2 (MkT @a) = \(MkT @a) -> MkT @a

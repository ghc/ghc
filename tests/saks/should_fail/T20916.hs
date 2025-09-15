{-# LANGUAGE CUSKs, EmptyDataDecls, PolyKinds, KindSignatures, StandaloneKindSignatures #-}

module T20916 where

import Data.Kind

type T3 :: k -> k -> Type
data T3 (a :: p) (b :: q) = MkT
-- Should fail because p and q are bound the same kind variable

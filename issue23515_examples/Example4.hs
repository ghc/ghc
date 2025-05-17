{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds #-}

module Example4 where

-- Current behavior
type family Foo a :: k
type instance Foo Int = Bool

-- With proposed change
{-
type family Foo' a :: k
type instance Foo' @Type Int = Bool
-}
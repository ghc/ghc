{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators, AllowAmbiguousTypes          #-}

module T17186 where

-- This test is significantly abbreviated from what was posted; see
-- #16512 for more context.

type family Dim v

type family v `OfDim` (n :: Dim v) = r | r -> n

(!*^) :: Dim m `OfDim` j -> Dim m `OfDim` i
(!*^) = undefined

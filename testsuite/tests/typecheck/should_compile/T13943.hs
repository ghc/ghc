{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE GADTs                   #-}

module Data.List.Unrolled where

import GHC.TypeLits

-- | Drop @n@ elements from a list
class Drop (n :: Nat) where
    drop :: [a] -> [a]

instance {-# OVERLAPPING #-} Drop 0 where
    drop xs = xs
    {-# INLINE drop #-}

instance {-# OVERLAPPABLE #-} (Drop (n - 1)) => Drop n where
    drop [] = []
    drop (_ : xs) = drop @(n - 1) xs
    {-# INLINE drop #-}

-- | Take @n@ elements from a list
class Take (n :: Nat) where
    take :: [a] -> [a]

instance {-# OVERLAPPING #-} Take 0 where
    take _ = []
    {-# INLINE take #-}

instance {-# OVERLAPPABLE #-} (Take (n - 1)) => Take n where
    take [] = []
    take (x : xs) = x : take @(n - 1) xs
    {-# INLINE take #-}

-- | Split list at @n@-th element.
splitAt :: forall (n :: Nat) a. (Take n, Drop n) => [a] -> ([a], [a])
splitAt xs = (take @n xs, drop @n xs)

-- | Split list into chunks of the given length @c@. @n@ is length of the list.
class ChunksOf (n :: Nat) (c :: Nat) where
    chunksOf :: [a] -> [[a]]

instance {-# OVERLAPPING #-} ChunksOf 0 0 where
    chunksOf _ = []
    {-# INLINE chunksOf #-}

instance {-# OVERLAPPABLE #-} ChunksOf 0 c where
    chunksOf _ = []
    {-# INLINE chunksOf #-}

instance {-# OVERLAPPABLE #-} ChunksOf n 0 where
    chunksOf _ = []
    {-# INLINE chunksOf #-}


instance {-# OVERLAPPABLE #-} (Take c, Drop c, ChunksOf (n - 1) c) => ChunksOf n c where
    chunksOf xs =
        let (l, r) = splitAt @c xs
        in l : chunksOf @(n - 1) @c r
    {-# INLINE chunksOf #-}

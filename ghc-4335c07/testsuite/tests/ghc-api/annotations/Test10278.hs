{-# LANGUAGE ScopedTypeVariables,GADTs #-}
module Test10278 where

extremumNewton :: forall tag. forall tag1. tag -> tag1 -> Int
extremumNewton = undefined

extremumNewton1 :: (Eq a, Fractional a) =>
                  (forall tag. forall tag1.
                          Tower tag1 (Tower tag a)
                              -> Tower tag1 (Tower tag a))
                      -> a -> [a]
extremumNewton1 f x0 = zeroNewton (diffUU f) x0

data MaybeDefault v where
    SetTo :: forall v . ( Eq v, Show v ) => !v -> MaybeDefault v
    SetTo2:: forall v . ( Eq v, Show v ) => !v -> MaybeDefault v
    SetTo3 :: (Eq a) => forall v . ( Eq v, Show v ) => !v -> a -> MaybeDefault v
    {-
    SetTo4 :: forall v . (( Eq v, Show v ) => v -> MaybeDefault v -> a -> [a])
    -}

{-# LANGUAGE RankNTypes #-}

module ShouldFail where

bar :: Num (forall a. a) => Int -> Int
bar = error "urk"


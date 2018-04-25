{-# LANGUAGE GADTs #-}

module T12083b where

class Class a where
    test :: a -> (Eq a => r) -> r

data P a b where
    Con :: (Class a, a ~ b) => P a b

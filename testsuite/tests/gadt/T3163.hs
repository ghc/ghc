{-# LANGUAGE GADTs, RankNTypes, ImpredicativeTypes #-}

-- Test Trac #3163

module Report where

data Taker a where
    Unreached :: Taker (forall s. s)


{-# LANGUAGE GADTs #-}
module T13300 where

data W where
  WI :: Int
  WD :: Double

data Superblock
  = A { f :: W }
  | B { f :: W }

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module T19849 where

data T where
  C :: forall k. T

f :: T -> ()
f C = ()

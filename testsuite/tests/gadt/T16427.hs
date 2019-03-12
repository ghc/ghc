{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module T16427 where

data D1 where C1 :: Int -> forall b . b -> D1
data D2 where C2 :: forall a . Int -> forall b . b -> D2

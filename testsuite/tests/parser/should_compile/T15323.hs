{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module T15323 where

data MaybeDefault v where
    TestParens  :: forall v . (Eq v) => MaybeDefault v

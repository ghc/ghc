{-# LANGUAGE ExperimentalDependentTypes #-}
module TypeLambdaPrint where

type L = Map (\ {- binder -} f  -> {- body -} f Int)
             [Maybe, Either String, IO]

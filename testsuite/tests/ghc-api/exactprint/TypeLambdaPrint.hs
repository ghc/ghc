{-# LANGUAGE ExperimentalDependentTypes #-}
module TypeLambdaPrint where

type L = Map (\ {- binder -} f  -> {- body -} f Int)
             [Maybe, Either String, IO]

type Multiple = (\a {- next binder -} b -> Either a b)
type Adjacent = (\a -> (\b -> Either a b))

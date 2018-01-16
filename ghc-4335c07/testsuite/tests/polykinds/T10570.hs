{-# LANGUAGE FunctionalDependencies, PolyKinds, FlexibleInstances #-}

module T10570 where

import Data.Proxy

class ConsByIdx2 x a m cls | x -> m where
    consByIdx2 :: x -> a -> m cls

instance ConsByIdx2 Int a Proxy cls where
    consByIdx2 _ _ = Proxy

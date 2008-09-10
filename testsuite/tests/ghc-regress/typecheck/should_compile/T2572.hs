 {-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

-- Trac #2572

module Foo where

type GTypeFun = forall a . a -> ()

gmapType :: Int -> GTypeFun
gmapType _ (_ :: a) = undefined

{-# LANGUAGE GADTSyntax, TemplateHaskell, KindSignatures #-}

module T5882 where

import Data.Kind (Type)

data Foo :: Type -> Type where
   Foo :: a -> Foo a

$( [d|  data Bar :: Type -> Type where
           Bar :: a -> Bar a
  |] )

f (Bar x) = Foo x

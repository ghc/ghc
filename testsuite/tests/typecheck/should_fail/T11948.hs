{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, NoMonoLocalBinds #-}
-- The NoMonoLocalBinds is crucial to making inference fail
-- See #11948 comment:2
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module T11948 where

type family F b

newtype Foo r = Foo r

type instance F (Foo r) = Foo (F r)

class Bar a b where
  bar :: a -> b

instance (Bar a b) => Bar (Foo a) (Foo b)

bug :: forall zq. (Bar (Foo (F zq)) (Foo zq))
               => Foo (F zq) -> Foo zq
bug sk = let x = bar sk :: Foo zq
         in x

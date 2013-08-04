{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Roles3 where

class C1 a where
  meth1 :: a -> a

class C2 a b where
  meth2 :: a ~ b => a -> b

class C3 a b where
  type F3 b
  meth3 :: a -> F3 b -> F3 b

type family F4 a

class C4 a b where
  meth4 :: a -> F4 b -> F4 b

type Syn1 a = F4 a
type Syn2 a = [a]
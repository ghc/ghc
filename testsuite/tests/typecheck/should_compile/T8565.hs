{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}
module Foo where

class C a where op :: (forall b. b -> a) -> a

newtype T x = MkT x deriving( C )

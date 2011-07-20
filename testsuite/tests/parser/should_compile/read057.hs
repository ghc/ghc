
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Foo where

class C a
instance C Int

newtype Foo = Foo Int
    deriving C


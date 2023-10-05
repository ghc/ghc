{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Foo where

class Foo x where
  foo :: x

bar :: (Foo (), _) => f ()
bar = pure foo

marie :: (Foo x, _) => f x
marie = pure foo

anne :: _ => f x
anne = pure foo

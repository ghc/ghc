{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Foo where


$([d| f :: forall a. a->a
      f x = x::a 
    |])

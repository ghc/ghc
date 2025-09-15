{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE NoImplicitPrelude #-}

module T24084_A (Foo (..)) where

class Foo a where
  type Bar a

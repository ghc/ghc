{-# LANGUAGE TemplateHaskell #-}

module T9066 where

$([d| data Blargh = (:<=>) Int Int
      infix 4 :<=>

      type Foo a b = Either a b
      infix 5 `Foo`
        |])

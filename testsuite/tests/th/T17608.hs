{-# LANGUAGE TemplateHaskell #-}
module T17608 where

$([d| infixl 4 `f`
      f :: Bool
      f = let infixl 4 `h`
              h :: () -> Bool -> Bool
              h _ _ = True in
          h () (g () ())
        where
        infixl 4 `g`
        g :: () -> () -> Bool
        g _ _ = True

      infixl 4 `n`
      class C a where
        infixl 4 `m`
        m :: a -> a -> a
        n :: a -> a -> a
    |])

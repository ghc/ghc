{-# LANGUAGE TypeOperators #-}

module B where
import A

a :: Int :+ Float :+ Double
a = undefined

b :: Int
b = case a of (p,q) -> p


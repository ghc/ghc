{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Main where

import Prelude hiding (sum)
import Data.Foldable

-- Derive Foldable for a simple data type

data List a = Nil | Cons a (List a)
    deriving (Functor,Foldable,Show)

someList = Cons 1 (Cons 1 (Cons 2 (Cons 3 Nil)))

main = print (sum someList)

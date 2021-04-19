{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module T19682 where

import Data.Kind
import Data.Proxy

convert :: (AllEq xs ys) => Proxy xs -> Proxy ys
convert p = Proxy

-- Works with ghc up to 9.0. Fails with ghc 9.2.
test :: Proxy '[Char, Bool] -> ()
test xs = const () (convert xs)

class (AllEqF xs ys, SameShapeAs xs ys) => AllEq (xs :: [a]) (ys :: [a])
instance (AllEqF xs ys, SameShapeAs xs ys) => AllEq xs ys

type family SameShapeAs (xs :: [a]) (ys :: [a]) :: Constraint where
  SameShapeAs '[]      ys = (ys ~ '[])
  SameShapeAs (x : xs) ys = (ys ~ (Head ys : Tail ys))

type family AllEqF (xs :: [a]) (ys :: [a]) :: Constraint where
  AllEqF '[]      '[]      = ()
  AllEqF (x : xs) (y : ys) = (x ~ y, AllEq xs ys)

type family Head (xs :: [a]) :: a where
  Head (x : xs) = x

type family Tail (xs :: [a]) :: [a] where
  Tail (x : xs) = xs

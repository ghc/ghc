{-# LANGUAGE TypeFamilies,
             BlockArguments,
             QualifiedDo,
             DataKinds,
             RequiredTypeArguments,
             ImpredicativeTypes,
             TypeAbstractions #-}

module Main where

import Data.Kind

-- This test case collects ALL recent features

main :: IO ()
main = demo False

demo :: Bool -> IO ()
demo b = Main.do -- this do-notation doesn't work without recent patch that expands HsDo at renamer
  x <- doubleOrInt b -- doesn't check without impredicative types
  let v = x + 1
  print v

(>>=) = ($)

doubleOrInt ::  Bool -> forall r. (forall a. (Num a, Show a) => a -> r) -> r
doubleOrInt True  f = wrap [Num, Show] @Double 4.15 \ @a -> f -- hack to convert (Num a, (Show a, ())) into (Num a, Show a)
doubleOrInt False f = f @Int 14 -- but you can simply use `f`

-- This function without RequiredTypeArguments would have ambiguous type
wrap :: forall c -> forall a. Constraints c a => a -> (forall a. Constraints c a => a -> r) -> r
wrap _ a f = f a

type Constraints :: [Type -> Constraint] -> Type -> Constraint
type family Constraints c a where
 Constraints '[] _ = ()
 Constraints (c : cs) a = (c a, Constraints cs a)

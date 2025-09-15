{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Main where

{- ghc-7.8.4 and ghc-7.10.2 showed a confusing warning:

T10767.hs:43:1: Warning:
    RULE left-hand side too complicated to desugar
      Optimised lhs: case cobox_aWY
                     of _ [Occ=Dead] { GHC.Types.Eq# cobox ->
                     genLength @ Int $dSpecList_aWX
                     }
      Orig lhs: case cobox_aWY of cobox_aWY { GHC.Types.Eq# cobox ->
                genLength @ Int $dSpecList_aWX
                }
-}

import Data.Proxy
import Data.Kind (Type)

class SpecList a where
    type List a :: Type

    slCase      :: List a -> b -> (a -> List a -> b) -> b

data IntList
  = ILNil
  | ILCons {-# UNPACK #-} !Int IntList
  deriving (Show)

instance SpecList Int where
  type List Int = IntList

  slCase ILNil        n _  = n
  slCase (ILCons i t) _ c  = c i t

fromList :: [Int] -> IntList
fromList []      = ILNil
fromList (h : t) = ILCons h (fromList t)

lst1 :: IntList
lst1 = fromList [1..10]

{-# SPECIALIZE genLength :: Proxy Int -> List Int -> Int #-}
genLength :: forall a . SpecList a => Proxy a -> List a -> Int
genLength p lst = slCase lst 0 (\(_ :: a) tail -> 1 + genLength p tail)

main :: IO ()
main = print (genLength (Proxy :: Proxy Int) lst1)

{-# LANGUAGE PolyKinds, TypeFamilies #-}

-- | Test equality predicates of Type.Reflection.
module Main where

import Type.Reflection
import Data.Kind
import Data.Maybe
import Data.Proxy
import Data.Functor.Const
import Data.Functor.Product

--data Product (f :: k -> Type) (g :: k -> Type) (a :: k)
--    = Product (f x) (g x)

test1 :: IO ()
test1 = do
    let x = typeRep :: TypeRep (Maybe String)
        y = typeRep :: TypeRep (Maybe Int)

    checkEq False x y
    App maybe1 _ <- pure x
    App maybe2 _ <- pure y
    checkEq True maybe1 maybe2


test2 :: IO ()
test2 = do
    let x = typeRep :: TypeRep (Proxy String)
        y = typeRep :: TypeRep (Proxy Int)

    checkEq False x y
    App proxy1 _ <- pure x
    App proxy2 _ <- pure y
    checkEq True proxy1 proxy2


test3 :: IO ()
test3 = do
    let x = typeRep :: TypeRep (Product (Const String) (Const Int) Int)
        y = typeRep :: TypeRep (Product (Const String) (Const Char) Int)
    checkEq False x y
    App dx _ <- pure x   -- "d" stands for decomposed
    App dy _ <- pure y
    checkEq False dx dy
    App ddx _ <- pure dx
    App ddy _ <- pure dy
    checkEq True ddx ddy


test4 :: IO ()
test4 = do
    let x = typeRep :: TypeRep (Product (Const String) (Const Int) Int)
        y = typeRep :: TypeRep (Product (Const String) (Const Int) Char)

    checkEq False x y
    App dx _ <- pure x
    App dy _ <- pure y
    checkEq True dx dy
    App ddx _ <- pure dx
    App ddy _ <- pure dy
    checkEq True ddx ddy


main :: IO ()
main = sequence_ [test1, test2, test3, test4]

type IsEqual = Bool

check :: Bool -> String -> IO ()
check success msg = putStrLn $ goodBad ++ " " ++ msg
  where goodBad
          | success    = "good"
          | otherwise  = "bad "

checkEq :: IsEqual -> TypeRep a -> TypeRep b -> IO ()
checkEq expected a b =
    check success (show a ++ " == " ++ show b ++ "?")
  where success = isJust (a `eqTypeRep` b) == expected

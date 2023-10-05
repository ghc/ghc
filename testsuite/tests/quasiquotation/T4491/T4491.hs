{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH.Quote

import qualified A as B

test1 :: [Int]
test1 = $(dataToExpQ (const Nothing) [1 :: Int, 2, 3])

test2 :: ()
test2 = $(dataToExpQ (const Nothing) ())

test3 :: (Int, Int, Int)
test3 = $(dataToExpQ (const Nothing) (1 :: Int, 2 :: Int, 3 :: Int))

test4 :: Rational
test4 = $(dataToExpQ (const Nothing) (5.5 :: Rational))

test5 :: B.Foo
test5 = $(dataToExpQ (const Nothing) (B.Foo 1))

main :: IO ()
main = do
    print test1
    print test2
    print test3
    print test4
    print test5

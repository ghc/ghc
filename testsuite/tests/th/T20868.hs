{-# LANGUAGE TemplateHaskell, GADTs #-}
module Main where

import Language.Haskell.TH


main :: IO ()
main = do
  x <- [d| newtype MyType where MyCons :: Bool -> MyType
           newtype MyType1 where MyCons1 :: Show a => a -> Bool -> MyType
           newtype MyType2 a where MyCons2 :: a -> MyType
           newtype MyType3 a where MyCons3 :: a -> MyType
           newtype MyType4 = (:#) Int
           newtype MyType5 where (:##) :: Int -> MyType
           |]
  putStrLn $ pprint x


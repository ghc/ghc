{-# LANGUAGE Arrows, LambdaCase #-}
module Main (main) where

import Control.Arrow

main :: IO ()
main = do
  putStrLn $ foo (Just 42)
  putStrLn $ foo (Just 500)
  putStrLn $ foo Nothing

foo :: ArrowChoice p => p (Maybe Int) String
foo = proc x ->
  (| id (\case
     Just x | x > 100   -> returnA -< "big " ++ show x
            | otherwise -> returnA -< "small " ++ show x
     Nothing            -> returnA -< "none")
  |) x

foo :: ArrowChoice p => p (Maybe Int) String
foo = proc x ->
  (| id (\cases
     y (Just x) | x > 100   -> returnA -< "big " ++ show x
                | otherwise -> returnA -< "small " ++ show x
     _ Nothing              -> returnA -< "none")
  |) 1 x

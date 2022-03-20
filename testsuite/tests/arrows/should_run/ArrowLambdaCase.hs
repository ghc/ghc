{-# LANGUAGE Arrows, LambdaCase #-}
module Main (main) where

import Control.Arrow
import Data.Function

main :: IO ()
main = do
  putStrLn $ foo (Just 42)
  putStrLn $ foo (Just 500)
  putStrLn $ foo Nothing
  putStrLn $ map ($ Just 42) [foo, bar] & \cases
    [foo', bar'] | foo' == bar' -> "success!"
                 | otherwise    -> error "failed"
  putStrLn $ baz 12 1 (Just 42)

foo :: ArrowChoice p => p (Maybe Int) String
foo = proc x ->
  (| id (\case
     Just x | x > 100   -> returnA -< "big " ++ show x
            | otherwise -> returnA -< "small " ++ show x
     Nothing            -> returnA -< "none")
  |) x

bar :: ArrowChoice p => p (Maybe Int) String
bar = proc x ->
  (| id (\cases
     (Just x) | x > 100   -> returnA -< "big " ++ show x
              | otherwise -> returnA -< "small " ++ show x
     Nothing              -> returnA -< "none")
  |) x

baz :: ArrowChoice p => Int -> Int -> p (Maybe Int) String
baz a b = proc x ->
  (| id (\cases
     (Just x) 12 20 | x > 100   -> returnA -< "big " ++ show x
                    | otherwise -> returnA -< "small " ++ show x
     Nothing _ _                -> returnA -< "none"
     _ 12 1                     -> returnA -< "less than none")
  |) x a b

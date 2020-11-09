{-# LANGUAGE MagicHash #-}
module TestUtils where

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual a b
  | a /= b = error (show a ++ " /= " ++ show b)
  | otherwise = return ()

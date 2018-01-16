{-# LANGUAGE ScopedTypeVariables #-}
module Test10396 where

errors :: IO ()
errors= do
  let ls :: Int = undefined
  return ()

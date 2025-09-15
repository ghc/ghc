{-# LANGUAGE TemplateHaskell #-}


module T25240a
  ( foo, func
  ) where


foo :: [a]
foo = []

foreign import ccall "func"
  func :: Int -> Int

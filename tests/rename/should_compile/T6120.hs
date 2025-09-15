module T6120 where

infix 3 +++ 

class C a where
  (+++) :: a -> a -> a

{-# DEPRECATED fail "fail is deprecated" #-}
fail :: String -> String
fail = id

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.String

-- {{{ Num literals
newtype N = N Int deriving (Show,Eq)

instance Num N where
  fromInteger 0 = error "0"
  fromInteger 1 = N 0
  fromInteger _ = N 1

f x = case x of
        1 -> False
        0 -> True

g x = case x of
        1 -> False
        _ -> case x of
              0 -> True
              _ -> error "No match"

testNum = do
  print $ g (N 0)
  print $ f (N 0)

-- }}}

-- {{{ IsString literals
newtype S = S String deriving Eq

instance IsString S where 
  fromString []    = error "[]"
  fromString (_:_) = S "."

fs x = case x of
        "." -> False
        ""  -> True

gs x = case x of
        "." -> False
        _   -> case x of
                 "" -> True
                 _  -> error "No match"

testIsString = do
  print $ gs (S ".")
  print $ fs (S ".")

-- }}}

main = do { testNum; testIsString }


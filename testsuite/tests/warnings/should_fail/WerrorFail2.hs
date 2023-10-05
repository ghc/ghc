{-# OPTIONS_GHC -Wall
                -Werror=incomplete-patterns
                -Werror=missing-fields #-}

module Werror03 where

data Rec = Rec
  { f1 :: Int
  , f2 :: Int
  } deriving (Show)

data S = C1 Int | C2 Int

-- incomplete pattern
sInt s = case s of
           C1 i -> i

-- missing field
printRec = print Rec{ f1 = 1 }

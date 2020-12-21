{-# LANGUAGE Incomplete #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wmissing-fields #-}
{-# OPTIONS_GHC -Wmissing-methods #-}

-- This test exercises each of the warnings the completeness checker accepts today

module A where

-- this should generate incomplete-patterns warning
foo :: Maybe a -> ()
foo Nothing = ()

data S = C1 Int | C2 Int

-- incomplete pattern
sInt s = case s of
           C1 i -> i

-- defer-missing-fields
data Rec = Rec
  { f1 :: Int
  , f2 :: Int
  } deriving (Show)

-- missing field
printRec = print Rec{ f1 = 1 }


-- defer-incomplete-uni-patterns
-- should warn about non-exhaustive pattern match
w :: String -> String
w x = let (_:_) = x in "1"

-- defer-missing-methods
class C a where
  m1 :: a
  m2 :: a

instance C Bool where
  m1 = False

data PartialRec = No
                | Yes { a :: Int, b :: Bool }

update r = r { b = False }

-- incomplete-patterns
myAbs :: Int -> Int
myAbs x | x < 0 = negate x

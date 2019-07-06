module T16804a where

import Data.Monoid

data Test = A | B
  deriving (Show)
instance Monoid Test where
  mempty = A
-- empty for linenumbers in T16804 to be correct
-- empty for linenumbers in T16804 to be correct

testFunction :: Test -> Test -> Bool
testFunction A B = True
testFunction B A = True
testFunction _ _ = False

testFunction2 :: Bool -> Test
testFunction2 True = A
testFunction2 False = B

niceValue :: Int
niceValue = getSum (Sum 1 <> Sum 2 <> mempty)

niceValue2 :: Test
niceValue2 = A <> A <> A <> B <> A <> mempty

instance Semigroup Test where
  A <> val = val
  B <> _   = B

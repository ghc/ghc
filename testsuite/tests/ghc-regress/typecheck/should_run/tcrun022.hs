-- Test the implementation of negative patterns.
-- We should get a call to 'negate'.

module Main where

main = print (minusTwo,trueOrFalse)

minusTwo = -2::N

trueOrFalse =
    case minusTwo of
      -2 -> True
      _ -> False

data N = Negate N | FromInteger Integer deriving (Eq,Show)

instance Num N where
  negate = Negate
  fromInteger = FromInteger

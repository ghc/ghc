-- !!! Test that 'negate' is used properly to construct negative literals

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

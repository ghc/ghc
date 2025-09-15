{-# LANGUAGE LexicalNegation #-}

data FreeNum
  = FromInteger Integer
  | FromRational Rational
  | Negate FreeNum
  | FreeNum `Subtract` FreeNum
  deriving (Show)

instance Num FreeNum where
  fromInteger = FromInteger
  negate = Negate
  (-) = Subtract

instance Fractional FreeNum where
  fromRational = FromRational

main = do
  print (-123 :: FreeNum)
  print (-1.5 :: FreeNum)
  print (let x = 5 in -x :: FreeNum)
  print (5-1 :: FreeNum)  -- unlike NegativeLiterals, we parse it as (5 - 1), not (5 (-1))
  print (-0 :: FreeNum)
  print (-0.0 :: FreeNum)
  print (-0o10 :: FreeNum)
  print (-0x10 :: FreeNum)

{-# LANGUAGE LexicalNegation, NegativeLiterals #-}

-- LexicalNegation vs NegativeLiterals

data FreeNum
  = FromInteger Integer
  | Negate FreeNum
  deriving (Show)

instance Num FreeNum where
  fromInteger = FromInteger
  negate = Negate

main = do
  let x = 5
  print (-123 :: FreeNum)
  print (-x :: FreeNum)

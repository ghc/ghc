module Lib where

data Value = Finite Integer | Infinity
  deriving (Eq)

instance Num Value where
  (+)         = undefined
  (*)         = undefined
  abs         = undefined
  signum      = undefined
  negate      = undefined
  fromInteger = Finite

-- | @litCon _@ should not elicit an overlapping patterns warning when it
-- passes through the LitCon case.
litCon :: Value -> Bool
litCon Infinity = True
litCon 0        = True
litCon _        = False

-- | @conLit Infinity@ should not elicit an overlapping patterns warning when
-- it passes through the ConLit case.
conLit :: Value -> Bool
conLit 0        = True
conLit Infinity = True
conLit _        = False

-- !!! Duplicate derived instance
module M where
data T = C deriving (Eq)
instance Eq T
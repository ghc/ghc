-- !!! Duplicate derived instance
module M where
class C a
data T = K deriving (C)

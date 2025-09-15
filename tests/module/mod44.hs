-- !!! Repeated instance decl
module M where
data T = T Int
instance Eq T
instance Eq T

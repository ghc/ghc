--!!! Type synonym in instance
module M where
type T = S
data S = MkS
instance Eq T

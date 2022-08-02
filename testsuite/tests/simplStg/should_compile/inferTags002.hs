module M where

data T a = MkT !Bool !a

-- The rhs of the case alternative should not result in a call std_ap_0_fast.
f x = case x of
    MkT y z -> z

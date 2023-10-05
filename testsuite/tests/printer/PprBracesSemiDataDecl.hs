
data Nat (t :: NatKind) where
{
    ZeroNat :: Nat Zero;
    SuccNat :: Nat t -> Nat (Succ t);
};

{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs #-}
module Linear10 where

data Unrestricted a where Unrestricted :: a -> Unrestricted a

unrestrictedDup :: Unrestricted a ⊸ (a, a)
unrestrictedDup (Unrestricted a) = (a,a)

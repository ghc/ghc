{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GADTs #-}
module Linear10 where

data Unrestricted a where Unrestricted :: a -> Unrestricted a

unrestrictedDup :: Unrestricted a ⊸ (a, a)
unrestrictedDup (Unrestricted a) = (a,a)

newtype Unrestricted2 a where Unrestricted2 :: a -> Unrestricted2 a

unrestricted2Dup :: Unrestricted2 a ⊸ (a,a)
unrestricted2Dup (Unrestricted2 a) = (a,a)

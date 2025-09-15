{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Linear11 where

data Unrestricted a where Unrestricted :: a -> Unrestricted a

incorrectUnrestricted :: a ⊸ Unrestricted a
incorrectUnrestricted a = Unrestricted a

data NotUnrestricted a where NotUnrestricted :: a ⊸ NotUnrestricted a

incorrectUnrestrictedDup :: NotUnrestricted a ⊸ (a,a)
incorrectUnrestrictedDup (NotUnrestricted a) = (a,a)

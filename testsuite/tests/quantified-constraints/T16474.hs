{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.Exts (Constraint)

class (forall a. A t a => A t [a]) => B t where
    type A t a :: Constraint

instance B t => B [t] where
    type A [t] a = A t a

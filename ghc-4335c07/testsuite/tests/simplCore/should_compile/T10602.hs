{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# OPTIONS_GHC -fno-spec-constr #-} -- Makes the problem go away.
-- {-# OPTIONS_GHC -fspec-constr-count=1 #-} -- Makes the problem go away.

module T10602 where

-- Copy-pasting T10602b.hs into the current module makes the problem go away.
import T10602b

data PairS a = PairS a a

-- Removing the '~' makes the problem go away.
(PairS _ _) >> ~(PairS b g) = PairS b g

class Binary t where
    put :: t -> PairS ()

-- Not using a newtype makes the problem go away.
newtype A a = A [a]

instance Binary a => Binary (A a) where
    put (A xs) = case splitAt 254 xs of
        (_, []) -> foldr (>>) (PairS () ()) (map put xs)
        (_, b)  -> put (A b)

{-# LANGUAGE TemplateHaskell #-}

module GadtConSigs_th_dump1 where

-- At the moment, the foralls and contexts are "flattened" during splicing,
-- e.g. `forall a. forall b.` turn into `forall a b.` and `Eq a => Show a =>`
-- turn into `(Eq a, Show a) =>` in the -ddump-splices output.
--
-- We could take more care to preserve the nested structure in GHC.ThToHs.cvtConstr,
-- but it does not seem to matter in practice.
$([d| data G a where
        G1 :: forall a b. (a, b) -> G a
        G2 :: forall a. forall b. (a, b) -> G a
        G3 :: forall a {b}. forall c d. forall e {f} g. G (a, b, c, d, e, f, g)
        -- G4 :: forall a -> G a   -- Uncomment later (see T25127_fail_th_quote)
        G5 :: (Eq a, Show a) => a -> G a
        G6 :: Eq a => Show a => a -> G a
        -- G7 :: forall a. Eq a => forall b. Show b => (a, b) -> G a   -- Uncomment later (#18389)
    |])
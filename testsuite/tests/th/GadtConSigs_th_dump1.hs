{-# LANGUAGE TemplateHaskell #-}

module GadtConSigs_th_dump1 where

-- At the moment, the foralls are "flattened" during splicing,
-- e.g. `forall a. forall b.` turn into `forall a b.` in the -ddump-splices output.
--
-- We could take more care to preserve the nested structure in GHC.ThToHs.cvtConstr,
-- but it does not seem to matter in practice.
$([d| data G a where
        G1 :: forall a b. (a, b) -> G a
        G2 :: forall a. forall b. (a, b) -> G a
        G3 :: forall a {b}. forall c d. forall e {f} g. G (a, b, c, d, e, f, g)
        -- G4 :: forall a -> G a   -- Uncomment later (see T25127_fail_th_quote)
    |])
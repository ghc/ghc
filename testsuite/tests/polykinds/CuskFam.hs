{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds #-}
-- {-# LANGUAGE CUSKs #-}    -- enabled by default

module CuskFam where

type family F a   -- non-injective

type family X :: F a
    -- Used to fail with:
    --
    --    • Couldn't match expected kind ‘F a1’ with actual kind ‘F a’
    --      NB: ‘F’ is a non-injective type family
    --      The type variable ‘a1’ is ambiguous
    --    • In the type family declaration for ‘X’
    --
    -- See Note [Unifying implicit CUSK variables] in GHC.Tc.Gen.HsType

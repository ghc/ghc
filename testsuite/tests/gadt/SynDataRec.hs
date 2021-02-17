{-# LANGUAGE KindSignatures, DataKinds, GADTs #-}

module SynDataRec where

-- This mutual recursion between a data type and
-- a type synonym is a little delicate.  See
-- Note [GADT return types] in GHC.Tc.TyCl

data Pass = Parsed | Renamed

data GhcPass (c :: Pass) where
  GhcPs :: GhcPs
  GhcRn :: GhcRn

type GhcPs   = GhcPass 'Parsed
type GhcRn   = GhcPass 'Renamed

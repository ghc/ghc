module GHC.Core.Opt.CompUnit
  ( parMapCompUnits
  ) where

import GHC.Prelude

import GHC.Conc (par, pseq)

import GHC.Core
import GHC.Core.Seq (seqBinds)

parMapCompUnits :: (CoreCompUnit -> CoreCompUnit) -> CoreProgram -> CoreProgram
parMapCompUnits f = go
  where
    go [] = []
    go (unit:units) = unit' `par` (units' `pseq` (unit' : units'))
      where
        unit' = forceCompUnit (f unit)
        units' = go units

    forceCompUnit unit@(CoreCompUnit unit_binds _unit_rules) =
      seqBinds unit_binds `seq` unit

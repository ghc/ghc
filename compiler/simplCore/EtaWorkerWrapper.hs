module EtaWorkerWrapper (etaArityWorkerWrapperProgram) where

import GhcPrelude

import CoreEta
import CoreSyn
import DynFlags ( DynFlags )
import UniqSupply
import Outputable
import PprCore

etaArityWorkerWrapperProgram
  :: DynFlags -> UniqSupply -> CoreProgram -> CoreProgram
etaArityWorkerWrapperProgram _dflags us binds
  -- = initUs_ us $ concat <$> mapM arityWorkerWrapper binds
  = panic (showSDocUnsafe (pprCoreBindingsWithSize (initUs_ us $ concat <$> mapM arityWorkerWrapper binds)))

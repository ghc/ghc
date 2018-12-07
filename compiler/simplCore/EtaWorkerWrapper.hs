module EtaWorkerWrapper (etaArityWorkerWrapperProgram) where

import GhcPrelude

import CoreEta
import CoreSyn
import DynFlags ( DynFlags )

etaArityWorkerWrapperProgram :: DynFlags -> CoreProgram -> CoreProgram
etaArityWorkerWrapperProgram _dflags binds = concatMap arityWorkerWrapper binds

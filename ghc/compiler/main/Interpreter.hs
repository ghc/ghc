-----------------------------------------------------------------------------
-- $Id: Interpreter.hs,v 1.10 2000/12/18 15:18:11 simonmar Exp $
--
-- Interpreter subsystem wrapper
--
-- (c) The University of Glasgow 2000
--
-----------------------------------------------------------------------------

module Interpreter (
#ifdef GHCI
	module ByteCodeGen,
	module Linker
#else
    ClosureEnv, emptyClosureEnv, 
    ItblEnv, emptyItblEnv,
    byteCodeGen,
    HValue,
    UnlinkedBCO, UnlinkedBCOExpr,
    loadObjs, resolveObjs,
#endif
  ) where

#ifdef GHCI

-- ------------------------------------------------------------
-- YES!  We have an interpreter
-- ------------------------------------------------------------

import ByteCodeGen
import Linker

#else

import Outputable

-- ------------------------------------------------------------
-- NO!  No interpreter; generate stubs for all the bits
-- ------------------------------------------------------------

type ClosureEnv = ()
emptyClosureEnv = ()

type ItblEnv = ()
emptyItblEnv = ()

type HValue          = ()
data UnlinkedBCO     = UnlinkedBCO
data UnlinkedBCOExpr = UnlinkedBCOExpr

instance Outputable UnlinkedBCO where
  ppr x = text "Can't output UnlinkedBCO"

byteCodeGen    = error "stgBindsToInterpSyn"
loadObjs       = error "loadObjs"
resolveObjs    = error "loadObjs"
interactiveUI  = error "interactiveUI"
#endif

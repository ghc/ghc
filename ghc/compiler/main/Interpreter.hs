-----------------------------------------------------------------------------
-- $Id: Interpreter.hs,v 1.12 2002/09/13 15:02:35 simonpj Exp $
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

extendLinkEnv xs = return ()

type ItblEnv = ()
emptyItblEnv = ()

type HValue          = ()
data UnlinkedBCO     = UnlinkedBCO
data UnlinkedBCOExpr = UnlinkedBCOExpr

instance Outputable UnlinkedBCO where
  ppr x = text "Can't output UnlinkedBCO"

byteCodeGen    = error "byteCodeGen"
loadObjs       = error "loadObjs"
resolveObjs    = error "resolveObjs"
interactiveUI  = error "interactiveUI"
#endif

-----------------------------------------------------------------------------
-- $Id: Interpreter.hs,v 1.9 2000/12/18 12:43:04 sewardj Exp $
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
    linkIModules,
    stgExprToInterpSyn, stgBindsToInterpSyn,
    HValue,
    UnlinkedIBind, UnlinkedIExpr,
    loadObjs, resolveObjs,
#endif
  ) where

#ifdef GHCI

---------------------------------------------
--	YES!  We have an interpreter
---------------------------------------------

import ByteCodeGen
import Linker

#else

import Outputable

---------------------------------------------
--	NO!  No interpreter; generate stubs for all the bits

---------------------------------------------

type ClosureEnv = ()
emptyClosureEnv = ()

type ItblEnv = ()
emptyItblEnv = ()

type HValue        = ()
data UnlinkedIBind = UnlinkedIBind
data UnlinkedIExpr = UnlinkedIExpr

instance Outputable UnlinkedIBind where
  ppr x = text "Can't output UnlinkedIBind"

linkIModules	    = error "linkIModules"
stgExprToInterpSyn  = error "stgToInterpSyn"
stgBindsToInterpSyn = error "stgBindsToInterpSyn"
loadObjs	    = error "loadObjs"
resolveObjs	    = error "loadObjs"
interactiveUI       = error "interactiveUI"
#endif

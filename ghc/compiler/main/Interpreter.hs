-----------------------------------------------------------------------------
-- $Id: Interpreter.hs,v 1.8 2000/11/20 16:28:29 simonmar Exp $
--
-- Interpreter subsystem wrapper
--
-- (c) The University of Glasgow 2000
--
-----------------------------------------------------------------------------

module Interpreter (
#ifdef GHCI
	module StgInterp,
	module InterpSyn,
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

import StgInterp
import InterpSyn
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

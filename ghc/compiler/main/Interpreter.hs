-----------------------------------------------------------------------------
-- $Id: Interpreter.hs,v 1.1 2000/11/07 16:03:38 simonmar Exp $
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
    ClosureEnv, ItblEnv,
    linkIModules,
    stgToInterpSyn,
    HValue,
    UnlinkedIBinds,
    loadObjs, resolveObjs,
#endif
  ) where

#ifdef GHCI
import StgInterp
import InterpSyn
import Linker
#else
type ClosureEnv = ()
type ItblEnv = ()
linkIModules = error "linkIModules"
stgToInterpSyn = error "linkIModules"
type HValue = ()
type UnlinkedIBinds = ()
loadObjs = error "loadObjs"
resolveObjs = error "loadObjs"
#endif

-----------------------------------------------------------------------------
-- $Id: Interpreter.hs,v 1.2 2000/11/08 13:51:58 simonmar Exp $
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
    UnlinkedIBind,
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
type UnlinkedIBind = ()
loadObjs = error "loadObjs"
resolveObjs = error "loadObjs"
#endif

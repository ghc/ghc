-----------------------------------------------------------------------------
-- $Id: Interpreter.hs,v 1.3 2000/11/08 14:52:06 simonpj Exp $
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
    stgToInterpSyn,
    HValue,
    UnlinkedIBind,
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

instance Outputable UnlinkedIBind where
  ppr x = text "Can't output UnlinkedIBind"

linkIModules	= error "linkIModules"
stgToInterpSyn	= error "linkIModules"
loadObjs	= error "loadObjs"
resolveObjs	= error "loadObjs"
#endif

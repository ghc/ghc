%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Typecheck]{Outside-world interfaces to the typechecker}

\begin{code}
#include "HsVersions.h"

module Typecheck (
	typecheckModule,

	-- and to make the interface self-sufficient...
	Module, Bag, CE(..), Binds, FixityDecl, E, Expr, InPat,
	RenamedPat(..), TypecheckedPat, Id, Inst, Maybe, MaybeErr,
	Name, PprStyle, PrettyRep, ProtoName, Error(..), Pretty(..),
	InstInfo, SplitUniqSupply, GlobalSwitch, UniqFM
    ) where

import TcMonad		-- typechecking monad machinery
import AbsSyn		-- the stuff being typechecked

import E		( nullE, E )
import Maybes		( MaybeErr(..) )
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import TcModule		-- tcModule, and associated stuff
import Util		-- for pragmas only
\end{code}

The typechecker stuff lives inside a complicated world of @TcM@
monadery.  This module provides three interfaces into that world, one
for typechecking a module, another for typechecking an expression, and
one for typechecking an interface.  This last one works as if
@typecheckModule@ was applied to the very simple module:
\begin{verbatim}
module EmptyModule where

import InterfaceOfInterest
\end{verbatim}
This is used when we want to augment an @E@ with information from an
interface.  (Used in the interpreter.)

\begin{code}
typecheckModule ::
       (GlobalSwitch -> Bool)	-- cmd-line switch checker
    -> SplitUniqSupply		-- name supply in
    -> GlobalNameFuns		-- renamer info (for doing derivings)
    -> RenamedModule		-- input module
	
    ->	------- OUTPUTS -----------
	-- depends v much on whether typechecking succeeds or not!
    MaybeErr
       -- SUCCESS ...
       (((TypecheckedBinds,	-- binds from class decls; does NOT
				--    include default-methods bindings
	 TypecheckedBinds,	-- binds from instance decls; INCLUDES
				--    class default-methods binds
	 TypecheckedBinds,	-- binds from value decls
	 [(Inst, TypecheckedExpr)]),

	([RenamedFixityDecl],	-- things for the interface generator
	 [Id],			-- to look at...
	 CE,
	 TCE,
	 Bag InstInfo),

	FiniteMap TyCon [(Bool, [Maybe UniType])],
				-- source tycon specialisation requests

--UNUSED:	E,			-- new cumulative E (with everything)
	E,			-- E just for stuff from THIS module
		-- NB: if you want the diff between two prev Es: i.e.,
		-- things in cumulative E that were added because of
		-- this module's import-ery, just do:
		--	bigE `minusE` thisModuleE

    	PprStyle->Pretty))	-- stuff to print for -ddump-deriving

       -- FAILURE ...
       (Bag Error)		-- pretty-print this to find out what went wrong

typecheckModule sw_chkr us renamer_name_funs modyule
  = initTc sw_chkr us (tcModule nullE renamer_name_funs modyule)
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Typecheck]{Outside-world interfaces to the typechecker}

\begin{code}
#include "HsVersions.h"

module Typecheck (
	typecheckModule, InstInfo
    ) where

import Ubiq
import TcMonad
import TcModule		( tcModule )
import TcInstUtil	( InstInfo )

import HsSyn
import RnHsSyn
import TcHsSyn

import ErrUtils		( TcWarning(..), TcError(..) )
import Pretty
import RnUtils		( GlobalNameMappers(..), GlobalNameMapper(..) )
import Maybes		( MaybeErr(..) )
\end{code}

The typechecker stuff lives inside a complicated world of @TcM@
monadery. 

ToDo: Interfaces for interpreter ...
	Typecheck an expression
	Typecheck an interface

\begin{code}
typecheckModule
    :: UniqSupply		-- name supply in
    -> GlobalNameMappers	-- renamer info (for doing derivings)
    -> RenamedHsModule		-- input module

    -> -- OUTPUTS ...
    MaybeErr
       -- SUCCESS ...
      (((TypecheckedHsBinds,	   -- binds from class decls; does NOT
				   --    include default-methods bindings
	 TypecheckedHsBinds,	   -- binds from instance decls; INCLUDES
				   --    class default-methods binds
	 TypecheckedHsBinds,	   -- binds from value decls

	 [(Id, TypecheckedHsExpr)] -- constant instance binds
	),

        ([RenamedFixityDecl], [Id], [TyCon], [Class], Bag InstInfo),
				-- things for the interface generator

        ([TyCon], [Class]),
				-- environments of info from this module only

	FiniteMap TyCon [(Bool, [Maybe Type])],
				-- source tycon specialisation requests

    	PprStyle->Pretty),	-- stuff to print for -ddump-deriving

       Bag TcWarning)		-- pretty-print this to get warnings

       -- FAILURE ...
      (Bag TcError,		-- pretty-print this to get errors
       Bag TcWarning)		-- pretty-print this to get warnings

typecheckModule us renamer_name_funs mod
  = initTc us (tcModule renamer_name_funs mod)
\end{code}

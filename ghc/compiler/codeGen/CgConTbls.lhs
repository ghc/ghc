%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CgConTbls]{Info tables and update bits for constructors}

\begin{code}
module CgConTbls ( genStaticConBits ) where

#include "HsVersions.h"

import AbsCSyn
import CgMonad

import StgSyn		( SRT(..) )
import AbsCUtils	( mkAbstractCs, mkAbsCStmts )
import CgTailCall	( performReturn, mkStaticAlgReturnCode )
import CLabel		( mkConEntryLabel )
import ClosureInfo	( layOutStaticClosure, layOutDynCon,
			  mkConLFInfo, ClosureInfo
			)
import CostCentre	( dontCareCCS )
import FiniteMap	( fmToList, FiniteMap )
import DataCon		( DataCon, dataConName, dataConRepArgTys, isNullaryDataCon )
import Name		( getOccString )
import PrimRep		( getPrimRepSize, PrimRep(..) )
import TyCon		( tyConDataCons, isEnumerationTyCon, TyCon )
import Type		( typePrimRep, Type )
import Outputable	
\end{code}

For every constructor we generate the following info tables:
	A static info table, for static instances of the constructor,

	Plus:

\begin{tabular}{lll}
Info tbls &	 Macro  &     	     Kind of constructor \\
\hline
info & @CONST_INFO_TABLE@&    Zero arity (no info -- compiler uses static closure)\\
info & @CHARLIKE_INFO_TABLE@& Charlike   (no info -- compiler indexes fixed array)\\
info & @INTLIKE_INFO_TABLE@&  Intlike; the one macro generates both info tbls\\
info & @SPEC_INFO_TABLE@&     SPECish, and bigger than or equal to @MIN_UPD_SIZE@\\
info & @GEN_INFO_TABLE@&      GENish (hence bigger than or equal to @MIN_UPD_SIZE@)\\
\end{tabular}

Possible info tables for constructor con:

\begin{description}
\item[@_con_info@:]
Used for dynamically let(rec)-bound occurrences of
the constructor, and for updates.  For constructors
which are int-like, char-like or nullary, when GC occurs,
the closure tries to get rid of itself.

\item[@_static_info@:]
Static occurrences of the constructor
macro: @STATIC_INFO_TABLE@.
\end{description}


For zero-arity constructors, \tr{con}, we NO LONGER generate a static closure;
it's place is taken by the top level defn of the constructor.

For charlike and intlike closures there is a fixed array of static
closures predeclared.

\begin{code}
genStaticConBits :: CompilationInfo 	-- global info about the compilation
		 -> [TyCon]		-- tycons to generate
		 -> AbstractC		-- output

genStaticConBits comp_info gen_tycons
  = -- for each type constructor:
    --	 grab all its data constructors;
    --	    for each one, generate an info table
    -- for each specialised type constructor
    --   for each specialisation of the type constructor
    --     grab data constructors, and generate info tables

    -- ToDo: for tycons and specialisations which are not
    --       declared in this module we must ensure that the
    --       C labels are local to this module i.e. static
    --	     since they may be duplicated in other modules

    mkAbstractCs [ gen_for_tycon tc | tc <- gen_tycons ]
  where
    gen_for_tycon :: TyCon -> AbstractC
    gen_for_tycon tycon
      = mkAbstractCs (map (genConInfo comp_info tycon) (tyConDataCons tycon)) 
 	`mkAbsCStmts` (
	  -- after the con decls, so we don't need to declare the constructor labels
	  if (isEnumerationTyCon tycon)
	    then CClosureTbl tycon
	    else AbsCNop
	)
\end{code}

%************************************************************************
%*									*
\subsection[CgConTbls-info-tables]{Generating info tables for constructors}
%*									*
%************************************************************************

Generate the entry code, info tables, and (for niladic constructor) the
static closure, for a constructor.

\begin{code}
genConInfo :: CompilationInfo -> TyCon -> DataCon -> AbstractC

genConInfo comp_info tycon data_con
  = mkAbstractCs [
		  CSplitMarker,
		  closure_code,
    	    	  static_code]
	-- Order of things is to reduce forward references
  where
    (closure_info, body_code) = mkConCodeAndInfo data_con

    -- To allow the debuggers, interpreters, etc to cope with static
    -- data structures (ie those built at compile time), we take care that
    -- info-table contains the information we need.
    (static_ci,_) = layOutStaticClosure con_name typePrimRep arg_tys 
				(mkConLFInfo data_con)

    body       = (initC comp_info (
	    	      profCtrC SLIT("TICK_ENT_CON") [CReg node] `thenC`
		      body_code))

    entry_addr = CLbl entry_label CodePtrRep
    con_descr  = getOccString data_con

    -- Don't need any dynamic closure code for zero-arity constructors
    closure_code = if zero_arity_con then 
			AbsCNop 
		   else 
			CClosureInfoAndCode closure_info body Nothing con_descr

    static_code  = CClosureInfoAndCode static_ci body Nothing con_descr

    cost_centre  = mkCCostCentreStack dontCareCCS -- not worried about static data costs

    zero_size arg_ty = getPrimRepSize (typePrimRep arg_ty) == 0

    zero_arity_con   = isNullaryDataCon data_con
	-- We used to check that all the arg-sizes were zero, but we don't
	-- really have any constructors with only zero-size args, and it's
	-- just one more thing to go wrong.

    arg_tys	    = dataConRepArgTys  data_con
    entry_label     = mkConEntryLabel      con_name
    con_name	    = dataConName data_con
\end{code}

\begin{code}
mkConCodeAndInfo :: DataCon		-- Data constructor
		 -> (ClosureInfo, Code)	-- The info table

mkConCodeAndInfo con
  = let
	arg_tys = dataConRepArgTys con

	(closure_info, arg_things)
		= layOutDynCon con typePrimRep arg_tys

	body_code
		= -- NB: We don't set CC when entering data (WDP 94/06)
		  profCtrC SLIT("TICK_RET_OLD") 
			[mkIntCLit (length arg_things)] `thenC`

		  performReturn AbsCNop		-- Ptr to thing already in Node
				(mkStaticAlgReturnCode con)
	in
	(closure_info, body_code)
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcInstUtil]{Utilities for typechecking instance declarations}

The bits common to TcInstDcls and TcDeriv.

\begin{code}
module TcInstUtil (
	InstInfo(..),
	buildInstanceEnv,
	classDataCon
    ) where

#include "HsVersions.h"

import RnHsSyn		( RenamedMonoBinds, RenamedSig )

import CmdLineOpts	( opt_AllowOverlappingInstances )
import TcMonad
import TcEnv		( InstEnv, emptyInstEnv, addToInstEnv )
import Bag		( bagToList, Bag )
import Class		( Class )
import Var		( TyVar, Id, idName )
import Maybes		( MaybeErr(..) )
import Name		( getSrcLoc, nameModule, isLocallyDefined )
import SrcLoc		( SrcLoc )
import Type		( Type, ClassContext )
import PprType		( pprConstraint )
import Class		( classTyCon )
import DataCon		( DataCon )
import TyCon		( tyConDataCons )
import Outputable
\end{code}

    instance c => k (t tvs) where b

\begin{code}
data InstInfo
  = InstInfo
      Class	        -- Class, k
      [TyVar]		-- Type variables, tvs
      [Type]		-- The types at which the class is being instantiated
      ClassContext	-- inst_decl_theta: the original context, c, from the
			--   instance declaration.  It constrains (some of)
			--   the TyVars above
      Id		-- The dfun id
      RenamedMonoBinds	-- Bindings, b
      SrcLoc		-- Source location assoc'd with this instance's defn
      [RenamedSig]	-- User pragmas recorded for generating specialised instances
\end{code}


%************************************************************************
%*									*
\subsection{Creating instance related Ids}
%*									*
%************************************************************************

A tiny function which doesn't belong anywhere else.
It makes a nasty mutual-recursion knot if you put it in Class.

\begin{code}
classDataCon :: Class -> DataCon
classDataCon clas = case tyConDataCons (classTyCon clas) of
		      (dict_constr:no_more) -> ASSERT( null no_more ) dict_constr 
\end{code}		      

%************************************************************************
%*									*
\subsection{Converting instance info into suitable InstEnvs}
%*									*
%************************************************************************

\begin{code}
buildInstanceEnv :: Bag InstInfo -> NF_TcM s InstEnv

buildInstanceEnv info = foldrNF_Tc addClassInstance emptyInstEnv (bagToList info)
\end{code}

@addClassInstance@ adds the appropriate stuff to the @ClassInstEnv@
based on information from a single instance declaration.  It complains
about any overlap with an existing instance.

\begin{code}
addClassInstance
    :: InstInfo
    -> InstEnv
    -> NF_TcM s InstEnv

addClassInstance 
    (InstInfo clas inst_tyvars inst_tys _
	      dfun_id _ src_loc _)
    inst_env
  = 	-- Add the instance to the class's instance environment
    case addToInstEnv opt_AllowOverlappingInstances 
		      inst_env clas inst_tyvars inst_tys dfun_id of
	Failed (tys', dfun_id')    -> addErrTc (dupInstErr clas (inst_tys, dfun_id) 
							        (tys',     dfun_id'))
						`thenNF_Tc_`
				     returnNF_Tc inst_env

	Succeeded inst_env' -> returnNF_Tc inst_env'
\end{code}

\begin{code}
dupInstErr clas info1@(tys1, dfun1) info2@(tys2, dfun2)
	-- Overlapping/duplicate instances for given class; msg could be more glamourous
  = hang (ptext SLIT("Duplicate or overlapping instance declarations"))
         4 (sep [ptext SLIT("for") <+> quotes (pprConstraint clas tys1),
		 nest 4 (sep [ppr_loc dfun1, ptext SLIT("and") <+> ppr_loc dfun2])])
  where
    ppr_loc dfun
	| isLocallyDefined dfun = ptext SLIT("defined at")  	     <+> ppr (getSrcLoc dfun)
	| otherwise		= ptext SLIT("imported from module") <+> quotes (ppr (nameModule (idName dfun)))
\end{code}

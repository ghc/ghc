%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcConDecls]{Typechecking @ConDecls@}

\begin{code}
#include "HsVersions.h"

module TcConDecls ( tcConDecls ) where

import TcMonad		-- typechecking monadic machinery
import AbsSyn

import CE		( CE(..) )
import E		( GVE(..), nullGVE, plusGVE )
import Errors		( confusedNameErr )
import Id		( mkDataCon, SpecEnv )
import TCE		( TCE(..), UniqFM )
import TVE		( TVE(..) )
import TcMonoType	( tcMonoType )
import Util
\end{code}

\begin{code}
tcConDecls :: TCE -> TVE -> TyCon -> [TyVarTemplate] -> SpecEnv
	   -> [RenamedConDecl] -> Baby_TcM GVE

tcConDecls tce tve tycon tyvars specenv [] = returnB_Tc nullGVE

tcConDecls tce tve tycon tyvars specenv (cd:cds) 
  = tc_decl cd					`thenB_Tc` \ gve_fst ->
    tcConDecls tce tve tycon tyvars specenv cds	`thenB_Tc` \ gve_rest ->
    returnB_Tc (plusGVE gve_fst gve_rest)
  where
    tc_decl (ConDecl name@(OtherTopId uniq full_name) tys src_loc)
      = addSrcLocB_Tc src_loc			 (
	mapB_Tc (tcMonoType fake_CE tce tve) tys `thenB_Tc` \ arg_tys ->
	returnB_Tc [(name, data_con arg_tys)]
    	)
      where
	fake_CE = panic "tcConDecls:CE"

	data_con arg_tys
	  = mkDataCon uniq
		      full_name
		      tyvars
		      [{-no context-}]
		      arg_tys
		      tycon
		      specenv

    tc_decl (ConDecl odd_name _ src_loc)
      = failB_Tc (confusedNameErr "Bad name for a data constructor (a Prelude name?)"
		    odd_name src_loc)
\end{code}

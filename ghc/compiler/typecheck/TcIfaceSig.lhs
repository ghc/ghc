%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
#include "HsVersions.h"

module TcIfaceSig ( tcInterfaceSigs ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Outputable
import Pretty

import TcMonad		-- typechecking monadic machinery
import AbsSyn		-- the stuff being typechecked

import AbsUniType	( splitType, splitTyArgs )
import CmdLineOpts	( GlobalSwitch(..) )
import E		( getE_CE, getE_TCE, nullGVE, unitGVE,
			  plusGVE, GVE(..), E, CE(..), TCE(..), UniqFM
			)
import Errors		( confusedNameErr )
import Id		-- mkImported
#if USE_ATTACK_PRAGMAS
import IdInfo		( workerExists )
#endif
import Maybes		( Maybe(..) )
import TcPragmas	( tcGenPragmas )
import TVE		( nullTVE, TVE(..) )
import TcPolyType	( tcPolyType )
import UniqFM		( emptyUFM ) -- profiling, pragmas only
import Util
\end{code}

Ultimately, type signatures in interfaces will have pragmatic
information attached, so it is a good idea to have separate code to
check them.

As always, we do not have to worry about user-pragmas in interface
signatures.

\begin{code}
tcInterfaceSigs :: E -> [RenamedSig] -> Baby_TcM GVE

tcInterfaceSigs e [] = returnB_Tc nullGVE

tcInterfaceSigs e (sig:sigs)
  = tc_sig	      sig  `thenB_Tc` \ gve1 ->
    tcInterfaceSigs e sigs `thenB_Tc` \ gve2 ->
    returnB_Tc (plusGVE gve1 gve2)
  where
    ce  = getE_CE  e
    tce = getE_TCE e

    tc_sig (Sig name@(OtherTopId uniq full_name) ty pragmas src_loc)
      = addSrcLocB_Tc src_loc			 (
	tcPolyType ce tce nullTVE ty	`thenB_Tc` \ sigma_ty ->

	fixB_Tc ( \ rec_imported_id ->
	    tcGenPragmas e (Just sigma_ty) rec_imported_id pragmas
				`thenB_Tc` \ id_info ->

	    returnB_Tc (mkImported uniq full_name sigma_ty id_info)
	) `thenB_Tc` \ final_id ->

	returnB_Tc (unitGVE name final_id)
	)

    tc_sig (Sig odd_name _ _ src_loc)
      = getSwitchCheckerB_Tc	`thenB_Tc` \ sw_chkr ->
	case odd_name of
	  WiredInVal _ | sw_chkr CompilingPrelude -- OK, that's cool; ignore
	    -> returnB_Tc nullGVE
	  _ -> failB_Tc (confusedNameErr "Bad name on a type signature (a Prelude name?)"
		        	odd_name src_loc)
\end{code}

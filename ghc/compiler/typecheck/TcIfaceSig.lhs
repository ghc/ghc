%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
#include "HsVersions.h"

module TcIfaceSig ( tcInterfaceSigs ) where

import Ubiq

import TcMonad		hiding ( rnMtoTcM )
import TcMonoType	( tcPolyType )

import HsSyn		( Sig(..), PolyType )
import RnHsSyn		( RenamedSig(..), RnName(..) )

import CmdLineOpts	( opt_CompilingPrelude )
import Id		( mkImported )
--import Name		( Name(..) )
import Pretty
import Util		( panic )


--import TcPragmas	( tcGenPragmas )
import IdInfo		( noIdInfo )
tcGenPragmas ty id ps = returnNF_Tc noIdInfo

\end{code}

Ultimately, type signatures in interfaces will have pragmatic
information attached, so it is a good idea to have separate code to
check them.

As always, we do not have to worry about user-pragmas in interface
signatures.

\begin{code}
tcInterfaceSigs :: [RenamedSig] -> TcM s [Id]

tcInterfaceSigs [] = returnTc []

tcInterfaceSigs (Sig name@(RnName full_name) ty pragmas src_loc : sigs)
  = tcAddSrcLoc src_loc		(
    tcPolyType ty		`thenTc` \ sigma_ty ->
    fixTc ( \ rec_id ->
	tcGenPragmas (Just sigma_ty) rec_id pragmas
				`thenNF_Tc` \ id_info ->
        returnTc (mkImported full_name sigma_ty id_info)
    ))				`thenTc` \ id ->
    tcInterfaceSigs sigs	`thenTc` \ sigs' ->
    returnTc (id:sigs')


tcInterfaceSigs (Sig odd_name _ _ src_loc : sigs)
  = case odd_name of
      WiredInId _ | opt_CompilingPrelude
        -> tcInterfaceSigs sigs
      _ -> tcAddSrcLoc src_loc	$
	   failTc (ifaceSigNameErr odd_name)

ifaceSigNameErr name sty
  = ppHang (ppStr "Bad name in an interface type signature (a Prelude name?)")
	 4 (ppr sty name)
\end{code}

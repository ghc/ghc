%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcIfaceSig]{Type checking of type signatures in interface files}

\begin{code}
#include "HsVersions.h"

module TcIfaceSig ( tcInterfaceSigs ) where

IMP_Ubiq()

import TcMonad		hiding ( rnMtoTcM )
import TcMonoType	( tcPolyType )

import HsSyn		( Sig(..), PolyType )
import RnHsSyn		( RenamedSig(..), RnName(..) )

import CmdLineOpts	( opt_CompilingPrelude )
import Id		( mkImported )
--import Name		( Name(..) )
import Maybes		( maybeToBool )
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

tcInterfaceSigs (Sig name ty pragmas src_loc : sigs)
  | has_full_name
  = tcAddSrcLoc src_loc		(
    tcPolyType ty		`thenTc` \ sigma_ty ->
    fixTc ( \ rec_id ->
	tcGenPragmas (Just sigma_ty) rec_id pragmas
				`thenNF_Tc` \ id_info ->
        returnTc (mkImported full_name sigma_ty id_info)
    ))				`thenTc` \ id ->
    tcInterfaceSigs sigs	`thenTc` \ sigs' ->
    returnTc (id:sigs')

  | otherwise -- odd name...
  = case name of
      WiredInId _ | opt_CompilingPrelude
        -> tcInterfaceSigs sigs
      _ -> tcAddSrcLoc src_loc	$
	   failTc (ifaceSigNameErr name)
  where
    has_full_name    = maybeToBool full_name_maybe
    (Just full_name) = full_name_maybe
    full_name_maybe  = case name of
			 RnName     fn	-> Just fn
			 RnImplicit fn	-> Just fn
			 _		-> Nothing

ifaceSigNameErr name sty
  = ppHang (ppStr "Bad name in an interface type signature (a Prelude name?)")
	 4 (ppr sty name)
\end{code}

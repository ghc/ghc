%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TCE]{Type constructor environment}

\begin{code}
#include "HsVersions.h"

module TCE (
	TCE(..), UniqFM,
	nullTCE, unitTCE,
	rngTCE,
	lookupTCE,
	plusTCE, checkTypeCycles,
-- NOT REALLY USED: printTypeInfoForPop,

	-- and to make the interface self-sufficient...
	MaybeErr, Name, TyCon,
	Error(..), SrcLoc, Pretty(..), PrettyRep

	IF_ATTACK_PRAGMAS(COMMA emptyUFM COMMA plusUFM)
	IF_ATTACK_PRAGMAS(COMMA eltsUFM  COMMA singletonDirectlyUFM)
	IF_ATTACK_PRAGMAS(COMMA u2i)
   ) where

import AbsUniType	( getMentionedTyCons, isDataTyCon, getTyConDataCons,
			  TyCon, Arity(..), Class, UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpClass)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import Digraph		( topologicalSort )
import Errors		-- notably typeCycleErr
import Id		( getDataConArity, Id, DataCon(..) )
import Maybes		( Maybe(..), MaybeErr(..) )
import Name
import Outputable
import Pretty
import UniqFM		-- basic environment handling
import Unique		( Unique )
import Util
\end{code}

\begin{code}
--data TCE = MkTCE (UniqFM TyCon)
type TCE = UniqFM TyCon
#define MkTCE {--}
-- also killed instance TCE, exported non-abstractly

nullTCE :: TCE
nullTCE = MkTCE emptyUFM

unitTCE :: Unique -> TyCon -> TCE
unitTCE uniq tycon = MkTCE (singletonDirectlyUFM uniq tycon)

rngTCE :: TCE -> [TyCon]
rngTCE (MkTCE tce) = eltsUFM tce

lookupTCE :: TCE -> Name -> TyCon
lookupTCE (MkTCE tce) name
  = case name of
      WiredInTyCon tycon       -> tycon
      PreludeTyCon key _ _ _   -> case (lookupDirectlyUFM tce key) of
				    Just tycon -> tycon
				    Nothing    -> err_msg
      OtherTyCon uniq _ _ _ _  -> case (lookupDirectlyUFM tce uniq) of
				    Just tycon -> tycon
				    Nothing    -> err_msg
  where
    err_msg = error ("ERROR: in looking up a type constructor! "++(ppShow 80 (ppr PprDebug name))++"\n(This can happen if you use `-fno-implicit-prelude'\nor you hide or change the system's Prelude.hi in some way.\nA -fhaskell-1.3 flag, or lack thereof, can trigger this error.)\n")

plusTCE :: TCE -> TCE -> TCE
plusTCE (MkTCE tce1) (MkTCE tce2) = MkTCE (plusUFM tce1 tce2)
\end{code}

\begin{code}
checkTypeCycles :: TCE -> MaybeErr () Error
checkTypeCycles tce
 = case (topologicalSort (==) edges vertices) of
    Succeeded ordering -> Succeeded ()
    Failed cycles
         -> Failed (typeCycleErr (map (\ c -> map fmt_tycon c) cycles))
	      where
		fmt_tycon c = (ppr PprForUser c, getSrcLoc c)
   where
   vertices = [ vertex1 | (vertex1, vertex2) <- edges]
   edges = concat (map get_edges (rngTCE tce))
	    where
	    get_edges tycon = [(tycon, dep) | dep <- getMentionedTyCons tycon]
		-- Make an arc for every dependency
\end{code}

\begin{code}
{- NOT REALLY USED:
printTypeInfoForPop :: TCE -> Pretty

printTypeInfoForPop (MkTCE tce)
  = ppAboves [ pp_type tc | tc <- eltsUFM tce, isDataTyCon tc ]
  where
    pp_type tycon
      = ppBesides [
	    ppStr "data ",
	    ppr PprForUser tycon, ppSP,
	    ppInterleave ppSP (map pp_data_con (getTyConDataCons tycon)),
	    ppSemi
    	]
      where
	pp_data_con data_con
	  = ppCat [ppr PprForUser data_con, ppInt (getDataConArity data_con)]
-}
\end{code}

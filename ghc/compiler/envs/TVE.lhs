%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[TVE]{Type variable environment}

This environment is not part of the big one that is carried around
monadically.

\begin{code}
#include "HsVersions.h"

module TVE (
	TVE(..), UniqFM,

	mkTVE, nullTVE, unitTVE,
	lookupTVE, lookupTVE_NoFail, plusTVE,

	-- and to make the interface self-sufficient...
	Maybe, Name, TyVarTemplate, UniType

	IF_ATTACK_PRAGMAS(COMMA emptyUFM COMMA plusUFM)
	IF_ATTACK_PRAGMAS(COMMA eltsUFM  COMMA singletonDirectlyUFM)
	IF_ATTACK_PRAGMAS(COMMA u2i)
    ) where

import AbsUniType	( mkUserTyVarTemplate, mkTyVarTemplateTy,
			  getTyVar, TyVarTemplate, TyVar, Class,
			  ClassOp, Arity(..), TyCon,
			  TauType(..), UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpClass)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyVar)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import Maybes		( Maybe(..), MaybeErr(..) )
import Name
import Outputable	-- def of ppr
import Pretty		-- to pretty-print error messages
import UniqFM		-- basic environment handling
import Unique		( Unique )
import Util
\end{code}

\begin{code}
type TVE = UniqFM UniType
#define MkTVE {--}
-- also: export non-abstractly

mkTVE :: [Name] -> (TVE, [TyVarTemplate], [TauType])
mkTVE names
  = case (unzip3 (map mk_tve_one names)) of { (env, tyvars, tys) ->
    (MkTVE (listToUFM_Directly env), tyvars, tys) }
  where
    mk_tve_one (Short uniq short_name)
      = case (mkUserTyVarTemplate uniq short_name)  of { tyvar ->
	case (mkTyVarTemplateTy tyvar)		    of { ty ->
    	((uniq, ty), tyvar, ty) }}

nullTVE :: TVE
nullTVE = MkTVE emptyUFM

unitTVE u ty = MkTVE (singletonDirectlyUFM u ty)

lookupTVE :: TVE -> Name -> UniType
lookupTVE (MkTVE tve) (Short uniq short_name)
 = case (lookupDirectlyUFM tve uniq) of
     Just ty -> ty
     Nothing -> panic "lookupTVE!"

lookupTVE_NoFail (MkTVE tve) (Short uniq short_name)
 = lookupDirectlyUFM tve uniq

plusTVE :: TVE -> TVE -> TVE
plusTVE (MkTVE tve1) (MkTVE tve2) = MkTVE (plusUFM tve1 tve2)
\end{code}

\begin{code}
#include "HsVersions.h"

module TyVar (
	GenTyVar(..), TyVar(..),
	mkTyVar,
	getTyVarKind,		-- TyVar -> Kind
	cloneTyVar,

	alphaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,

	-- We also export "environments" keyed off of
	-- TyVars and "sets" containing TyVars:
	TyVarEnv(..),
	nullTyVarEnv, mkTyVarEnv, addOneToTyVarEnv,
	growTyVarEnvList, isNullTyVarEnv, lookupTyVarEnv,

	GenTyVarSet(..), TyVarSet(..),
	emptyTyVarSet, unitTyVarSet, unionTyVarSets,
	unionManyTyVarSets, intersectTyVarSets, mkTyVarSet,
	tyVarSetToList, elementOfTyVarSet, minusTyVarSet,
	isEmptyTyVarSet
  ) where

CHK_Ubiq() 	-- debugging consistency check
import IdLoop 	-- for paranoia checking

-- friends
import Usage		( GenUsage, Usage(..), usageOmega )
import Kind		( Kind, mkBoxedTypeKind )

-- others
import UniqSet		-- nearly all of it
import UniqFM		( emptyUFM, listToUFM, addToUFM, lookupUFM,
			  plusUFM, sizeUFM, UniqFM
			)
import Maybes		( Maybe(..) )
import Name		( mkLocalName, Name, RdrName(..) )
import Pretty		( Pretty(..), PrettyRep, ppBeside, ppPStr )
import PprStyle		( PprStyle )
--import Outputable	( Outputable(..), NamedThing(..), ExportFlag(..) )
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import Unique		( showUnique, mkAlphaTyVarUnique, Unique )
import Util		( panic, Ord3(..) )
\end{code}

\begin{code}
data GenTyVar flexi_slot
  = TyVar
	Unique
	Kind
	(Maybe Name)		-- User name (if any)
	flexi_slot		-- Extra slot used during type and usage
				-- inference, and to contain usages.

type TyVar = GenTyVar Usage	-- Usage slot makes sense only if Kind = Type
\end{code}


Simple construction and analysis functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
mkTyVar :: Name -> Unique -> Kind -> TyVar
mkTyVar name uniq kind = TyVar  uniq
				kind
				(Just name)
				usageOmega

getTyVarKind :: GenTyVar flexi -> Kind
getTyVarKind (TyVar _ kind _ _) = kind

cloneTyVar :: GenTyVar flexi -> Unique -> GenTyVar flexi
cloneTyVar (TyVar _ k n x) u = TyVar u k n x
\end{code}


Fixed collection of type variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
alphaTyVars = [ TyVar u mkBoxedTypeKind Nothing usageOmega
	      | u <- map mkAlphaTyVarUnique [1..] ]

(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars
\end{code}


Environments
~~~~~~~~~~~~
\begin{code}
type TyVarEnv elt = UniqFM elt

nullTyVarEnv	 :: TyVarEnv a
mkTyVarEnv	 :: [(GenTyVar flexi, a)] -> TyVarEnv a
addOneToTyVarEnv :: TyVarEnv a -> GenTyVar flexi -> a -> TyVarEnv a
growTyVarEnvList :: TyVarEnv a -> [(GenTyVar flexi, a)] -> TyVarEnv a
isNullTyVarEnv	 :: TyVarEnv a -> Bool
lookupTyVarEnv	 :: TyVarEnv a -> GenTyVar flexi -> Maybe a

nullTyVarEnv	 = emptyUFM
mkTyVarEnv	 = listToUFM
addOneToTyVarEnv = addToUFM
lookupTyVarEnv   = lookupUFM

growTyVarEnvList env pairs = plusUFM env (listToUFM pairs)
isNullTyVarEnv   env	   = sizeUFM env == 0
\end{code}

Sets
~~~~
\begin{code}
type GenTyVarSet flexi	= UniqSet (GenTyVar flexi)
type TyVarSet		= UniqSet TyVar

emptyTyVarSet     :: GenTyVarSet flexi
intersectTyVarSets:: GenTyVarSet flexi -> GenTyVarSet flexi -> GenTyVarSet flexi
unionTyVarSets    :: GenTyVarSet flexi -> GenTyVarSet flexi -> GenTyVarSet flexi
unionManyTyVarSets:: [GenTyVarSet flexi] -> GenTyVarSet flexi
tyVarSetToList    :: GenTyVarSet flexi -> [GenTyVar flexi]
unitTyVarSet :: GenTyVar flexi -> GenTyVarSet flexi
elementOfTyVarSet :: GenTyVar flexi -> GenTyVarSet flexi -> Bool
minusTyVarSet	  :: GenTyVarSet flexi -> GenTyVarSet flexi -> GenTyVarSet flexi
isEmptyTyVarSet   :: GenTyVarSet flexi -> Bool
mkTyVarSet	  :: [GenTyVar flexi] -> GenTyVarSet flexi

emptyTyVarSet  	  = emptyUniqSet
unitTyVarSet = unitUniqSet
intersectTyVarSets= intersectUniqSets
unionTyVarSets 	  = unionUniqSets
unionManyTyVarSets= unionManyUniqSets
tyVarSetToList 	  = uniqSetToList
elementOfTyVarSet = elementOfUniqSet
minusTyVarSet	  = minusUniqSet
isEmptyTyVarSet   = isEmptyUniqSet
mkTyVarSet	  = mkUniqSet
\end{code}

Instance delarations
~~~~~~~~~~~~~~~~~~~~
\begin{code}
instance Eq (GenTyVar a) where
    (TyVar u1 _ _ _) == (TyVar u2 _ _ _) = u1 == u2

instance Ord3 (GenTyVar a) where
    cmp (TyVar u1 _ _ _) (TyVar u2 _ _ _) = u1 `cmp` u2

instance Uniquable (GenTyVar a) where
    uniqueOf (TyVar u _ _ _) = u

instance NamedThing (GenTyVar a) where
    getName		(TyVar _ _ (Just n) _) = n
    getName		(TyVar u _ _        _) = mkLocalName u (showUnique u) mkUnknownSrcLoc
\end{code}

\begin{code}
module TyVar (
	GenTyVar(..), TyVar, 

	mkTyVar, mkSysTyVar,
	tyVarKind,		-- TyVar -> Kind
	cloneTyVar, nameTyVar,

	openAlphaTyVar,
	alphaTyVars, alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar,

	-- We also export "environments" keyed off of
	-- TyVars and "sets" containing TyVars:
	TyVarEnv,
	emptyTyVarEnv, mkTyVarEnv, zipTyVarEnv, addToTyVarEnv, plusTyVarEnv,
	growTyVarEnvList, isEmptyTyVarEnv, lookupTyVarEnv, delFromTyVarEnv,

	GenTyVarSet, TyVarSet,
	emptyTyVarSet, unitTyVarSet, unionTyVarSets,
	unionManyTyVarSets, intersectTyVarSets, mkTyVarSet,
	tyVarSetToList, elementOfTyVarSet, minusTyVarSet,
	isEmptyTyVarSet
  ) where

#include "HsVersions.h"

-- friends
import Kind		( Kind, mkBoxedTypeKind, mkTypeKind )

-- others
import UniqSet		-- nearly all of it
import UniqFM		( emptyUFM, listToUFM, addToUFM, lookupUFM,
			  plusUFM, sizeUFM, delFromUFM, UniqFM
			)
import BasicTypes	( Unused, unused )
import Name		( mkSysLocalName, mkLocalName, Name, NamedThing(..), OccName )
import SrcLoc		( noSrcLoc, SrcLoc )
import Unique		( mkAlphaTyVarUnique, Unique, Uniquable(..) )
import Util		( zipEqual )
import Outputable
\end{code}

\begin{code}
data GenTyVar flexi_slot
  = TyVar
	Unique
	Kind
	(Maybe Name)		-- User name (if any)
	flexi_slot		-- Extra slot used during type and usage
				-- inference, and to contain usages.

type TyVar   = GenTyVar Unused
\end{code}


Simple construction and analysis functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
mkTyVar :: Name -> Kind -> TyVar
mkTyVar name kind = TyVar  (uniqueOf name)
			   kind
			   (Just name)
			   unused

mkSysTyVar :: Unique -> Kind -> TyVar
mkSysTyVar uniq kind = TyVar uniq
			     kind
			     Nothing
			     unused

tyVarKind :: GenTyVar flexi -> Kind
tyVarKind (TyVar _ kind _ _) = kind

cloneTyVar :: GenTyVar flexi -> Unique -> GenTyVar flexi
cloneTyVar (TyVar _ k n x) u = TyVar u k Nothing x
	-- Zaps its name

nameTyVar :: GenTyVar flexi -> OccName -> GenTyVar flexi
	-- Give the TyVar a print-name
nameTyVar (TyVar u k n x) occ = TyVar u k (Just (mkLocalName u occ noSrcLoc)) x
\end{code}


Fixed collection of type variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
	-- openAlphaTyVar is prepared to be instantiated
	-- to a boxed or unboxed type variable.  It's used for the 
	-- result type for "error", so that we can have (error Int# "Help")
openAlphaTyVar = TyVar (mkAlphaTyVarUnique 1) mkTypeKind Nothing unused

alphaTyVars = [ TyVar u mkBoxedTypeKind Nothing unused
	      | u <- map mkAlphaTyVarUnique [2..] ]

(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar:_) = alphaTyVars

\end{code}


Environments
~~~~~~~~~~~~
\begin{code}
type TyVarEnv elt = UniqFM elt

emptyTyVarEnv	 :: TyVarEnv a
mkTyVarEnv	 :: [(GenTyVar flexi, a)] -> TyVarEnv a
zipTyVarEnv	 :: [GenTyVar flexi] -> [a] -> TyVarEnv a
addToTyVarEnv :: TyVarEnv a -> GenTyVar flexi -> a -> TyVarEnv a
growTyVarEnvList :: TyVarEnv a -> [(GenTyVar flexi, a)] -> TyVarEnv a
isEmptyTyVarEnv	 :: TyVarEnv a -> Bool
lookupTyVarEnv	 :: TyVarEnv a -> GenTyVar flexi -> Maybe a
delFromTyVarEnv	 :: TyVarEnv a -> GenTyVar flexi -> TyVarEnv a
plusTyVarEnv     :: TyVarEnv a -> TyVarEnv a -> TyVarEnv a

emptyTyVarEnv	 = emptyUFM
mkTyVarEnv	 = listToUFM
addToTyVarEnv    = addToUFM
lookupTyVarEnv   = lookupUFM
delFromTyVarEnv  = delFromUFM
plusTyVarEnv     = plusUFM

zipTyVarEnv tyvars tys     = listToUFM (zipEqual "zipTyVarEnv" tyvars tys)
growTyVarEnvList env pairs = plusUFM env (listToUFM pairs)
isEmptyTyVarEnv   env	   = sizeUFM env == 0
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

instance Ord (GenTyVar a) where
    compare (TyVar u1 _ _ _) (TyVar u2 _ _ _) = u1 `compare` u2

instance Uniquable (GenTyVar a) where
    uniqueOf (TyVar u _ _ _) = u

instance NamedThing (GenTyVar a) where
    getName (TyVar _ _ (Just n) _) = n
    getName (TyVar u _ _        _) = mkSysLocalName u SLIT("t") noSrcLoc
\end{code}

%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Class]{The @Class@ datatype}

\begin{code}
#include "HsVersions.h"

module Class (
	GenClass(..), SYN_IE(Class),

	mkClass,
	classKey, classSelIds, classDictArgTys,
	classSuperDictSelId, classDefaultMethodId,
	classBigSig, classInstEnv,
	isSuperClassOf,
	classOpTagByOccName,

	SYN_IE(ClassInstEnv)
    ) where

CHK_Ubiq() -- debugging consistency check

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(TyLoop)
IMPORT_DELOOPER(IdLoop)
#else
import {-# SOURCE #-} Id	( Id, idType, idName )
import {-# SOURCE #-} Type
import {-# SOURCE #-} TysWiredIn
import {-# SOURCE #-} TysPrim
#endif

#if __GLASGOW_HASKELL__ >= 202
import Name
#endif

import TyCon		( TyCon )
import TyVar		( SYN_IE(TyVar), GenTyVar )
import Usage		( GenUsage, SYN_IE(Usage), SYN_IE(UVar) )

import MatchEnv		( MatchEnv )
import Maybes		( assocMaybe )
import Name		( changeUnique, Name, OccName, occNameString )
import Unique		-- Keys for built-in classes
import Pretty		( Doc, hsep, ptext )
import SrcLoc		( SrcLoc )
import Outputable
import Util
\end{code}

%************************************************************************
%*									*
\subsection[Class-basic]{@Class@: basic definition}
%*									*
%************************************************************************

A @Class@ corresponds to a Greek kappa in the static semantics:

The parameterisation wrt tyvar and uvar is only necessary to
get appropriately general instances of Ord3 for GenType.

\begin{code}
data GenClass tyvar uvar
  = Class
	Unique		-- Key for fast comparison
	Name

	tyvar	  	-- The class type variable

	[GenClass tyvar uvar] 	-- Immediate superclasses, and the
	[Id]			-- corresponding selector functions to
				-- extract them from a dictionary of this
			  	-- class

	[Id]			 	  --	 * selector functions
	[Maybe Id]			  --	 * default methods
			  -- They are all ordered by tag.  The
			  -- selector ids are less innocent than they
			  -- look, because their IdInfos contains
			  -- suitable specialisation information.  In
			  -- particular, constant methods are
			  -- instances of selectors at suitably simple
			  -- types.

	ClassInstEnv	  -- Gives details of all the instances of this class

	[(GenClass tyvar uvar, [GenClass tyvar uvar])]
			  -- Indirect superclasses;
			  --   (k,[k1,...,kn]) means that
			  --   k is an immediate superclass of k1
			  --   k1 is an immediate superclass of k2
			  --   ... and kn is an immediate superclass
			  -- of this class.  (This is all redundant
			  -- information, since it can be derived from
			  -- the superclass information above.)

type Class        = GenClass TyVar UVar

type ClassInstEnv = MatchEnv Type Id		-- The Ids are dfuns
\end{code}

The @mkClass@ function fills in the indirect superclasses.

\begin{code}
mkClass :: Unique -> Name -> TyVar
	-> [Class] -> [Id]
	-> [Id] -> [Maybe Id]
	-> ClassInstEnv
	-> Class

mkClass uniq full_name tyvar super_classes superdict_sels
	dict_sels defms class_insts
  = Class uniq (changeUnique full_name uniq) tyvar
		super_classes superdict_sels
		dict_sels defms
		class_insts
		trans_clos
  where
    trans_clos :: [(Class,[Class])]
    trans_clos = transitiveClosure succ (==) [ (clas, []) | clas <- super_classes ]

    succ (clas@(Class _ _ _ super_classes _ _ _ _ _), links)
      = [(super, (clas:links)) | super <- super_classes]
\end{code}

%************************************************************************
%*									*
\subsection[Class-selectors]{@Class@: simple selectors}
%*									*
%************************************************************************

The rest of these functions are just simple selectors.

\begin{code}
classKey (Class key _ _ _ _ _ _ _ _) = key
classSelIds (Class _ _ _ _ _ sels _ _ _) = sels

classDefaultMethodId (Class _ _ _ _ _ _ defm_ids _ _) idx
  = defm_ids !! idx

classSuperDictSelId (Class _ _ _ scs scsel_ids _ _ _ _) super_clas
  = assoc "classSuperDictSelId" (scs `zip` scsel_ids) super_clas

classBigSig (Class _ _ tyvar super_classes sdsels sels defms _ _)
  = (tyvar, super_classes, sdsels, sels, defms)

classInstEnv (Class _ _ _ _ _ _ _ inst_env _) = inst_env

classDictArgTys :: Class -> Type -> [Type]	-- Types of components of the dictionary (C ty)
classDictArgTys (Class _ _ _ _ sc_sel_ids meth_sel_ids _ _ _) ty
  = map mk_arg_ty (sc_sel_ids ++ meth_sel_ids)
  where
    mk_arg_ty id = case splitRhoTy (applyTy (idType id) ty) of
			(sel_theta, meth_ty) -> ASSERT( length sel_theta == 1 )
		   				meth_ty

classOpTagByOccName clas occ
  = go (classSelIds clas) 1
  where
    go (sel_id : sel_ids) tag 
	    | getOccName (idName sel_id) == occ = tag
	    | otherwise		                = go sel_ids (tag+1)
    go [] _ = pprPanic "classOpTagByOccName"
		(hsep [ppr PprDebug (getName clas), ppr PprDebug occ])
\end{code}

@a `isSuperClassOf` b@ returns @Nothing@ if @a@ is not a superclass of
@b@, but if it is, it returns $@Just@~[k_1,\ldots,k_n]$, where the
$k_1,\ldots,k_n$ are exactly as described in the definition of the
@GenClass@ constructor above.

\begin{code}
isSuperClassOf :: Class -> Class -> Maybe [Class]
clas `isSuperClassOf` (Class _ _ _ _ _ _ _ _ links) = assocMaybe links clas
\end{code}

%************************************************************************
%*									*
\subsection[Class-instances]{Instance declarations for @Class@}
%*									*
%************************************************************************

We compare @Classes@ by their keys (which include @Uniques@).

\begin{code}
instance Ord3 (GenClass tyvar uvar) where
  cmp (Class k1 _ _ _ _ _ _ _ _) (Class k2 _ _ _ _ _ _ _ _)  = cmp k1 k2

instance Eq (GenClass tyvar uvar) where
    (Class k1 _ _ _ _ _ _ _ _) == (Class k2 _ _ _ _ _ _ _ _) = k1 == k2
    (Class k1 _ _ _ _ _ _ _ _) /= (Class k2 _ _ _ _ _ _ _ _) = k1 /= k2

instance Ord (GenClass tyvar uvar) where
    (Class k1 _ _ _ _ _ _ _ _) <= (Class k2 _ _ _ _ _ _ _ _) = k1 <= k2
    (Class k1 _ _ _ _ _ _ _ _) <  (Class k2 _ _ _ _ _ _ _ _) = k1 <  k2
    (Class k1 _ _ _ _ _ _ _ _) >= (Class k2 _ _ _ _ _ _ _ _) = k1 >= k2
    (Class k1 _ _ _ _ _ _ _ _) >  (Class k2 _ _ _ _ _ _ _ _) = k1 >  k2
    _tagCmp a b = case cmp a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
\end{code}

\begin{code}
instance Uniquable (GenClass tyvar uvar) where
    uniqueOf (Class u _ _ _ _ _ _ _ _) = u

instance NamedThing (GenClass tyvar uvar) where
    getName (Class _ n _ _ _ _ _ _ _) = n
\end{code}



%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Class]{The @Class@ datatype}

\begin{code}
#include "HsVersions.h"

module Class (
	GenClass(..), SYN_IE(Class),

	mkClass,
	classKey, classOps, classGlobalIds,
	classSuperDictSelId, classOpId, classDefaultMethodId,
	classSig, classBigSig, classInstEnv,
	isSuperClassOf,
	classOpTagByOccName, classOpTagByOccName_maybe,

	GenClassOp(..), SYN_IE(ClassOp),
	mkClassOp,
	classOpTag, classOpString,
	classOpLocalType,

	SYN_IE(ClassInstEnv)
    ) where

CHK_Ubiq() -- debugging consistency check

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(TyLoop)
#else
import {-# SOURCE #-} Id
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
import Outputable
import Unique		-- Keys for built-in classes
import UniqFM           ( SYN_IE(Uniquable))
import Pretty		( Doc, hsep, ptext )
import SrcLoc		( SrcLoc )
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
data GenClassOp ty
  = ClassOp	OccName	-- The operation name

		Int	-- Unique within a class; starts at 1

		ty	-- Type; the class tyvar is free (you can find
			-- it from the class). This means that a
			-- ClassOp doesn't make much sense outside the
			-- context of its parent class.

data GenClass tyvar uvar
  = Class
	Unique		-- Key for fast comparison
	Name

	tyvar	  	-- The class type variable

	[GenClass tyvar uvar] 	-- Immediate superclasses, and the
	[Id]			-- corresponding selector functions to
				-- extract them from a dictionary of this
			  	-- class

	[GenClassOp (GenType tyvar uvar)] -- The * class operations
	[Id]			 	  --	 * selector functions
	[Id]				  --	 * default methods
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
type ClassOp      = GenClassOp Type

type ClassInstEnv = MatchEnv Type Id		-- The Ids are dfuns
\end{code}

The @mkClass@ function fills in the indirect superclasses.

\begin{code}
mkClass :: Unique -> Name -> TyVar
	-> [Class] -> [Id]
	-> [ClassOp] -> [Id] -> [Id]
	-> ClassInstEnv
	-> Class

mkClass uniq full_name tyvar super_classes superdict_sels
	class_ops dict_sels defms class_insts
  = Class uniq (changeUnique full_name uniq) tyvar
		super_classes superdict_sels
		class_ops dict_sels defms
		class_insts
		trans_clos
  where
    trans_clos :: [(Class,[Class])]
    trans_clos = transitiveClosure succ (==) [ (clas, []) | clas <- super_classes ]

    succ (clas@(Class _ _ _ super_classes _ _ _ _ _ _), links)
      = [(super, (clas:links)) | super <- super_classes]
\end{code}

%************************************************************************
%*									*
\subsection[Class-selectors]{@Class@: simple selectors}
%*									*
%************************************************************************

The rest of these functions are just simple selectors.

\begin{code}
classKey (Class key _ _ _ _ _ _ _ _ _) = key
classOps (Class _ _ _ _ _ ops _ _ _ _) = ops
classGlobalIds (Class _ _ _ _ _ _ sels defm_ids _ _) = sels ++ defm_ids

classOpId (Class _ _ _ _ _ ops op_ids _ _ _) op
  = op_ids !! (classOpTag op - 1)

classDefaultMethodId (Class _ _ _ _ _ ops _ defm_ids _ _) idx
  = defm_ids !! idx

classSuperDictSelId (Class _ _ _ scs scsel_ids _ _ _ _ _) super_clas
  = assoc "classSuperDictSelId" (scs `zip` scsel_ids) super_clas

classSig :: GenClass t u -> (t, [GenClass t u], [GenClassOp (GenType t u)])
classSig (Class _ _ tyvar super_classes _ ops _ _ _ _)
  = (tyvar, super_classes, ops)

classBigSig (Class _ _ tyvar super_classes sdsels ops sels defms _ _)
  = (tyvar, super_classes, sdsels, ops, sels, defms)

classInstEnv (Class _ _ _ _ _ _ _ _ inst_env _) = inst_env
\end{code}

@a `isSuperClassOf` b@ returns @Nothing@ if @a@ is not a superclass of
@b@, but if it is, it returns $@Just@~[k_1,\ldots,k_n]$, where the
$k_1,\ldots,k_n$ are exactly as described in the definition of the
@GenClass@ constructor above.

\begin{code}
isSuperClassOf :: Class -> Class -> Maybe [Class]
clas `isSuperClassOf` (Class _ _ _ _ _ _ _ _ _ links) = assocMaybe links clas
\end{code}

%************************************************************************
%*									*
\subsection[Class-instances]{Instance declarations for @Class@}
%*									*
%************************************************************************

We compare @Classes@ by their keys (which include @Uniques@).

\begin{code}
instance Ord3 (GenClass tyvar uvar) where
  cmp (Class k1 _ _ _ _ _ _ _ _ _) (Class k2 _ _ _ _ _ _ _ _ _)  = cmp k1 k2

instance Eq (GenClass tyvar uvar) where
    (Class k1 _ _ _ _ _ _ _ _ _) == (Class k2 _ _ _ _ _ _ _ _ _) = k1 == k2
    (Class k1 _ _ _ _ _ _ _ _ _) /= (Class k2 _ _ _ _ _ _ _ _ _) = k1 /= k2

instance Ord (GenClass tyvar uvar) where
    (Class k1 _ _ _ _ _ _ _ _ _) <= (Class k2 _ _ _ _ _ _ _ _ _) = k1 <= k2
    (Class k1 _ _ _ _ _ _ _ _ _) <  (Class k2 _ _ _ _ _ _ _ _ _) = k1 <  k2
    (Class k1 _ _ _ _ _ _ _ _ _) >= (Class k2 _ _ _ _ _ _ _ _ _) = k1 >= k2
    (Class k1 _ _ _ _ _ _ _ _ _) >  (Class k2 _ _ _ _ _ _ _ _ _) = k1 >  k2
    _tagCmp a b = case cmp a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
\end{code}

\begin{code}
instance Uniquable (GenClass tyvar uvar) where
    uniqueOf (Class u _ _ _ _ _ _ _ _ _) = u

instance NamedThing (GenClass tyvar uvar) where
    getName (Class _ n _ _ _ _ _ _ _ _) = n

instance NamedThing (GenClassOp ty) where
    getOccName (ClassOp occ _ _) = occ
\end{code}


%************************************************************************
%*									*
\subsection[ClassOp-basic]{@ClassOp@: type and basic functions}
%*									*
%************************************************************************

A @ClassOp@ represents a a class operation.  From it and its parent
class we can construct the dictionary-selector @Id@ for the
operation/superclass dictionary, and the @Id@ for its default method.
It appears in a list inside the @Class@ object.

The type of a method in a @ClassOp@ object is its local type; that is,
without the overloading of the class itself.  For example, in the
declaration
\begin{pseudocode}
	class Foo a where
		op :: Ord b => a -> b -> a
\end{pseudocode}
the type recorded for @op@ in the @ClassOp@ list of the @Class@ object is
just
	$\forall \beta.~
		@Ord@~\beta \Rightarrow
		\alpha \rightarrow \beta \rightarrow alpha$

(where $\alpha$ is the class type variable recorded in the @Class@
object).  Of course, the type of @op@ recorded in the GVE will be its
``full'' type

	$\forall \alpha \forall \beta.~
		@Foo@~\alpha \Rightarrow
		~@Ord@~\beta \Rightarrow \alpha
		\rightarrow \beta \rightarrow alpha$

******************************************************************
**** That is, the type variables of a class op selector
***  are all at the outer level.
******************************************************************

\begin{code}
mkClassOp :: OccName -> Int -> ty -> GenClassOp ty
mkClassOp name tag ty = ClassOp name tag ty

classOpTag :: GenClassOp ty -> Int
classOpTag    (ClassOp _ tag _) = tag

classOpString :: GenClassOp ty -> FAST_STRING
classOpString (ClassOp occ _ _) = occNameString occ

classOpLocalType :: GenClassOp ty -> ty {-SigmaType-}
classOpLocalType (ClassOp _ _ ty) = ty
\end{code}

Rather unsavoury ways of getting ClassOp tags:
\begin{code}
classOpTagByOccName_maybe :: Class -> OccName -> Maybe Int
classOpTagByOccName       :: Class -> OccName -> Int

classOpTagByOccName clas op
  = case (classOpTagByOccName_maybe clas op) of
      Just tag -> tag
#ifdef DEBUG
      Nothing  -> pprPanic "classOpTagByOccName:" (hsep (ppr PprDebug op : map (ptext . classOpString) (classOps clas)))
#endif

classOpTagByOccName_maybe clas op
  = go (classOps clas) 1
  where
    go []     		      _   = Nothing
    go (ClassOp occ _ _ : ns) tag = if occ == op
				    then Just tag
				    else go ns (tag+1)
\end{code}

%************************************************************************
%*									*
\subsection[ClassOp-instances]{Instance declarations for @ClassOp@}
%*									*
%************************************************************************

@ClassOps@ are compared by their tags.

\begin{code}
instance Eq (GenClassOp ty) where
    (ClassOp _ i1 _) == (ClassOp _ i2 _) = i1 == i2
    (ClassOp _ i1 _) /= (ClassOp _ i2 _) = i1 == i2

instance Ord (GenClassOp ty) where
    (ClassOp _ i1 _) <= (ClassOp _ i2 _) = i1 <= i2
    (ClassOp _ i1 _) <  (ClassOp _ i2 _) = i1 <  i2
    (ClassOp _ i1 _) >= (ClassOp _ i2 _) = i1 >= i2
    (ClassOp _ i1 _) >  (ClassOp _ i2 _) = i1 >  i2
    -- ToDo: something for _tagCmp? (WDP 94/10)
\end{code}

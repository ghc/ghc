%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Class]{The @Class@ datatype}

\begin{code}
#include "HsVersions.h"

module Class (
	GenClass(..), Class(..),

	mkClass,
	getClassKey, getClassOps, getClassSelIds,
	getSuperDictSelId, getClassOpId, getDefaultMethodId,
	getClassSig, getClassBigSig, getClassInstEnv,
	isSuperClassOf,

	derivableClassKeys, cCallishClassKeys,
	isNumericClass, isStandardClass, isCcallishClass,

	GenClassOp(..), ClassOp(..),
	mkClassOp,
	getClassOpTag, getClassOpString,
	getClassOpLocalType,

	ClassInstEnv(..)

	-- and to make the interface self-sufficient...
    ) where

CHK_Ubiq() -- debugging consistency check

import TyLoop

import TyCon		( TyCon )
import TyVar		( TyVar(..), GenTyVar )
import Usage		( GenUsage, Usage(..), UVar(..) )

import Maybes		( assocMaybe, Maybe )
import NameTypes	( FullName, ShortName )
import Unique		-- Keys for built-in classes
import Outputable	( Outputable(..), NamedThing(..), ExportFlag )
import Pretty		( Pretty(..), PrettyRep )
import PprStyle		( PprStyle )
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
  = ClassOp	FAST_STRING -- The operation name

		Int	-- Unique within a class; starts at 1

		ty	-- Type; the class tyvar is free (you can find
			-- it from the class). This means that a
			-- ClassOp doesn't make much sense outside the
			-- context of its parent class.

data GenClass tyvar uvar
  = Class
	Unique		-- Key for fast comparison
	FullName

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
mkClass :: Unique -> FullName -> TyVar
	-> [Class] -> [Id]
	-> [ClassOp] -> [Id] -> [Id]
	-> ClassInstEnv
	-> Class

mkClass uniq full_name tyvar super_classes superdict_sels
	class_ops dict_sels defms class_insts
  = Class uniq full_name tyvar
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
getClassKey (Class key _ _ _ _ _ _ _ _ _) = key
getClassOps (Class _ _ _ _ _ ops _ _ _ _) = ops
getClassSelIds (Class _ _ _ _ _ _ sels _ _ _) = sels

getClassOpId (Class _ _ _ _ _ ops op_ids _ _ _) op
  = op_ids !! (getClassOpTag op - 1)
getDefaultMethodId (Class _ _ _ _ _ ops _ defm_ids _ _) op
  = defm_ids !! (getClassOpTag op - 1)
getSuperDictSelId (Class _ _ _ scs scsel_ids _ _ _ _ _) super_clas
  = assoc "getSuperDictSelId" (scs `zip` scsel_ids) super_clas

getClassSig :: GenClass t u -> (t, [GenClass t u], [GenClassOp (GenType t u)])
getClassSig (Class _ _ tyvar super_classes _ ops _ _ _ _)
  = (tyvar, super_classes, ops)

getClassBigSig (Class _ _ tyvar super_classes sdsels ops sels defms _ _)
  = (tyvar, super_classes, sdsels, ops, sels, defms)

getClassInstEnv (Class _ _ _ _ _ _ _ _ inst_env _) = inst_env
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
\subsection[Class-std-groups]{Standard groups of Prelude classes}
%*									*
%************************************************************************

@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@TcDeriv@).

NOTE: @Eq@ and @Text@ do need to appear in @standardClasses@
even though every numeric class has these two as a superclass,
because the list of ambiguous dictionaries hasn't been simplified.

\begin{code}
isNumericClass, isStandardClass :: Class -> Bool

isNumericClass   (Class key _ _ _ _ _ _ _ _ _) = key `is_elem` numericClassKeys
isStandardClass  (Class key _ _ _ _ _ _ _ _ _) = key `is_elem` standardClassKeys
isCcallishClass	 (Class key _ _ _ _ _ _ _ _ _) = key `is_elem` cCallishClassKeys
is_elem = isIn "is_X_Class"

numericClassKeys
  = [ numClassKey,
      realClassKey,
      integralClassKey,
      fractionalClassKey,
      floatingClassKey,
      realFracClassKey,
      realFloatClassKey ]

derivableClassKeys
  = [ eqClassKey,
      showClassKey,
      ordClassKey,
      enumClassKey,
      ixClassKey,
      readClassKey ]

cCallishClassKeys = [ cCallableClassKey, cReturnableClassKey ]

standardClassKeys
  = derivableClassKeys ++ numericClassKeys ++ cCallishClassKeys
    --
    -- We have to have "_CCallable" and "_CReturnable" in the standard
    -- classes, so that if you go...
    --
    --	    _ccall_ foo ... 93{-numeric literal-} ...
    --
    -- ... it can do The Right Thing on the 93.
\end{code}

%************************************************************************
%*									*
\subsection[Class-instances]{Instance declarations for @Class@}
%*									*
%************************************************************************

We compare @Classes@ by their keys (which include @Uniques@).

\begin{code}
instance Ord3 (GenClass tyvar uvar) where
  cmp (Class k1 _ _ _ _ _ _ _ _ _) (Class k2 _ _ _ _ _ _ _ _ _)
    = cmp k1 k2

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
instance NamedThing (GenClass tyvar uvar) where
    getExportFlag 	(Class _ n _ _ _ _ _ _ _ _) = getExportFlag n
    isLocallyDefined	(Class _ n _ _ _ _ _ _ _ _) = isLocallyDefined n
    getOrigName		(Class _ n _ _ _ _ _ _ _ _) = getOrigName n
    getOccurrenceName	(Class _ n _ _ _ _ _ _ _ _) = getOccurrenceName n
    getInformingModules	(Class _ n _ _ _ _ _ _ _ _) = getInformingModules n
    getSrcLoc		(Class _ n _ _ _ _ _ _ _ _) = getSrcLoc n
    fromPreludeCore	(Class _ n _ _ _ _ _ _ _ _) = fromPreludeCore n

    getItsUnique (Class key _ _ _ _ _ _ _ _ _) = key
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
mkClassOp :: FAST_STRING -> Int -> ty -> GenClassOp ty
mkClassOp name tag ty = ClassOp name tag ty

getClassOpTag :: GenClassOp ty -> Int
getClassOpTag    (ClassOp _ tag _) = tag

getClassOpString :: GenClassOp ty -> FAST_STRING
getClassOpString (ClassOp str _ _) = str

getClassOpLocalType :: GenClassOp ty -> ty {-SigmaType-}
getClassOpLocalType (ClassOp _ _ ty) = ty
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


%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Class]{The @Class@ datatype}

\begin{code}
#include "HsVersions.h"

module Class (
	Class(..),	-- must be *NON*-abstract so UniTyFuns can see it

	mkClass,
	getClassKey, getClassOps,
	getSuperDictSelId, getClassOpId, getDefaultMethodId,
	getConstMethodId,
	getClassSig, getClassBigSig, getClassInstEnv,
--UNUSED: getClassDefaultMethodsInfo,
	isSuperClassOf,
	cmpClass,

	derivableClassKeys,
	isNumericClass, isStandardClass, --UNUSED: isDerivableClass,

	ClassOp(..),	-- must be non-abstract so UniTyFuns can see them
	mkClassOp,
	getClassOpTag, getClassOpString,
--UNUSED: getClassOpSig,
	getClassOpLocalType,

	-- and to make the interface self-sufficient...
	Id, InstTemplate, Maybe, Name, FullName, TyVarTemplate,
	UniType, Unique
    ) where

import Id		( getIdSpecialisation, Id )
import IdInfo
import InstEnv		( ClassInstEnv(..), MatchEnv(..) )
import Maybes		( assocMaybe, Maybe(..) )
import Name		( Name(..), ShortName )
import NameTypes	( FullName, SrcLoc )
import Pretty
import Outputable	-- class for printing, forcing
import TyCon		( TyCon, Arity(..)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon)
			)
import TyVar		( TyVarTemplate )
import Unique		-- class key stuff
import UniType		( UniType, ThetaType(..), TauType(..)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import UniTyFuns	( splitType, pprClassOp
			  IF_ATTACK_PRAGMAS(COMMA pprUniType COMMA pprTyCon)
			)
import Util
\end{code}

%************************************************************************
%*									*
\subsection[Class-basic]{@Class@: basic definition}
%*									*
%************************************************************************

A @Class@ corresponds to a Greek kappa in the static semantics:

\begin{code}
data Class
  = MkClass 
	Unique{-ClassKey-}-- Key for fast comparison
	FullName

	TyVarTemplate	  -- The class type variable

	[Class] [Id]	  -- Immediate superclasses, and the
			  -- corresponding selector functions to
			  -- extract them from a dictionary of this
			  -- class

	[ClassOp] 	  -- The * class operations
	[Id]	 	  --	 * selector functions
	[Id]		  --	 * default methods
			  -- They are all ordered by tag.  The
			  -- selector ids are less innocent than they
			  -- look, because their IdInfos contains
			  -- suitable specialisation information.  In
			  -- particular, constant methods are
			  -- instances of selectors at suitably simple
			  -- types.

	ClassInstEnv	  -- Gives details of all the instances of this class

	[(Class,[Class])] -- Indirect superclasses;
			  --   (k,[k1,...,kn]) means that
			  --   k is an immediate superclass of k1
			  --   k1 is an immediate superclass of k2
			  --   ... and kn is an immediate superclass
			  -- of this class.  (This is all redundant
			  -- information, since it can be derived from
			  -- the superclass information above.)
\end{code}

The @mkClass@ function fills in the indirect superclasses.

\begin{code}
mkClass :: Name -> TyVarTemplate
	-> [Class] -> [Id]
	-> [ClassOp] -> [Id] -> [Id]
	-> ClassInstEnv
	-> Class

mkClass name tyvar super_classes superdict_sels
	class_ops dict_sels defms class_insts
  = MkClass key full_name tyvar
		super_classes superdict_sels
		class_ops dict_sels defms
		class_insts
		trans_clos
  where
    (key,full_name) = case name of
			OtherClass  uniq full_name _ -> (uniq, full_name)
			PreludeClass key full_name   -> (key,  full_name)

    trans_clos :: [(Class,[Class])]
    trans_clos = transitiveClosure succ (==) [ (clas, []) | clas <- super_classes ]

    succ (clas@(MkClass _ _ _ super_classes _ _ _ _ _ _), links) 
      = [(super, (clas:links)) | super <- super_classes]
\end{code}

%************************************************************************
%*									*
\subsection[Class-selectors]{@Class@: simple selectors}
%*									*
%************************************************************************

The rest of these functions are just simple selectors.

\begin{code}
getClassKey (MkClass key _ _ _ _ _ _ _ _ _) = key

getClassOps (MkClass _ _ _ _ _ ops _ _ _ _) = ops

getSuperDictSelId (MkClass _ _ _ scs scsel_ids _ _ _ _ _) super_clas
  = assoc "getSuperDictSelId" (scs `zip` scsel_ids) super_clas

getClassOpId (MkClass _ _ _ _ _ ops op_ids _ _ _) op
  = op_ids !! (getClassOpTag op - 1)

getDefaultMethodId (MkClass _ _ _ _ _ ops _ defm_ids _ _) op
  = defm_ids !! (getClassOpTag op - 1)

getConstMethodId (MkClass _ _ _ _ _ ops op_ids _ _ _) op ty
  = -- constant-method info is hidden in the IdInfo of
    -- the class-op id (as mentioned up above).
    let
	sel_id = op_ids !! (getClassOpTag op - 1)
    in
    case (lookupConstMethodId sel_id ty) of
      Just xx -> xx
      Nothing -> error (ppShow 80 (ppAboves [
	ppCat [ppStr "ERROR: getConstMethodId:", ppr PprDebug op, ppr PprDebug ty, ppr PprDebug ops, ppr PprDebug op_ids, ppr PprDebug sel_id],
	ppStr "(This can arise if an interface pragma refers to an instance",
	ppStr "but there is no imported interface which *defines* that instance.",
	ppStr "The info above, however ugly, should indicate what else you need to import."
	]))

getClassSig :: Class -> (TyVarTemplate, [Class], [ClassOp])

getClassSig (MkClass _ _ tyvar super_classes _ ops _ _ _ _)
  = (tyvar, super_classes, ops)

getClassBigSig (MkClass _ _ tyvar super_classes sdsels ops sels defms _ _)
  = (tyvar, super_classes, sdsels, ops, sels, defms)

getClassInstEnv (MkClass _ _ _ _ _ _ _ _ inst_env _) = inst_env

--UNUSED: getClassDefaultMethodsInfo (MkClass _ _ _ _ _ _ _ defms _ _) = defms
\end{code}

@a `isSuperClassOf` b@ returns @Nothing@ if @a@ is not a superclass of
@b@, but if it is, it returns $@Just@~[k_1,\ldots,k_n]$, where the
$k_1,\ldots,k_n$ are exactly as described in the definition of the
@MkClass@ constructor above.

\begin{code}
isSuperClassOf :: Class -> Class -> Maybe [Class]

clas `isSuperClassOf` (MkClass _ _ _ _ _ _ _ _ _ links) = assocMaybe links clas
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
isNumericClass, isStandardClass {-UNUSED:, isDerivableClass-} :: Class -> Bool

isNumericClass   (MkClass key _ _ _ _ _ _ _ _ _) = key `is_elem` numericClassKeys
isStandardClass  (MkClass key _ _ _ _ _ _ _ _ _) = key `is_elem` standardClassKeys
--isDerivableClass (MkClass key _ _ _ _ _ _ _ _ _) = key `is_elem` derivableClassKeys

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
      textClassKey, 
      ordClassKey, 
      enumClassKey, 
      ixClassKey ]
      -- ToDo: add binaryClass

standardClassKeys
  = derivableClassKeys ++ numericClassKeys
    ++ [ cCallableClassKey, cReturnableClassKey ]
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
cmpClass (MkClass k1 _ _ _ _ _ _ _ _ _) (MkClass k2 _ _ _ _ _ _ _ _ _)
  = cmpUnique k1 k2

instance Eq Class where
    (MkClass k1 _ _ _ _ _ _ _ _ _) == (MkClass k2 _ _ _ _ _ _ _ _ _) = k1 == k2
    (MkClass k1 _ _ _ _ _ _ _ _ _) /= (MkClass k2 _ _ _ _ _ _ _ _ _) = k1 /= k2

instance Ord Class where
    (MkClass k1 _ _ _ _ _ _ _ _ _) <= (MkClass k2 _ _ _ _ _ _ _ _ _) = k1 <= k2
    (MkClass k1 _ _ _ _ _ _ _ _ _) <  (MkClass k2 _ _ _ _ _ _ _ _ _) = k1 <  k2
    (MkClass k1 _ _ _ _ _ _ _ _ _) >= (MkClass k2 _ _ _ _ _ _ _ _ _) = k1 >= k2
    (MkClass k1 _ _ _ _ _ _ _ _ _) >  (MkClass k2 _ _ _ _ _ _ _ _ _) = k1 >  k2
#ifdef __GLASGOW_HASKELL__
    _tagCmp a b = case cmpClass a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
#endif
\end{code}

\begin{code}
instance NamedThing Class where
    getExportFlag 	(MkClass _ n _ _ _ _ _ _ _ _) = getExportFlag n
    isLocallyDefined	(MkClass _ n _ _ _ _ _ _ _ _) = isLocallyDefined n
    getOrigName		(MkClass _ n _ _ _ _ _ _ _ _) = getOrigName n
    getOccurrenceName	(MkClass _ n _ _ _ _ _ _ _ _) = getOccurrenceName n
    getInformingModules	(MkClass _ n _ _ _ _ _ _ _ _) = getInformingModules n
    getSrcLoc		(MkClass _ n _ _ _ _ _ _ _ _) = getSrcLoc n
    fromPreludeCore	(MkClass _ n _ _ _ _ _ _ _ _) = fromPreludeCore n

    getTheUnique = panic "NamedThing.Class.getTheUnique"
    hasType 	 = panic "NamedThing.Class.hasType"
    getType 	 = panic "NamedThing.Class.getType"
\end{code}

And the usual output stuff:
\begin{code}
instance Outputable Class where
    -- we use pprIfaceClass for printing in interfaces

{-  ppr sty@PprShowAll (MkClass u n _ _ _ ops _ _ _ _)
      = ppCat [ppr sty n, pprUnique u, ppr sty ops]
-}
    ppr sty (MkClass u n _ _ _ _ _ _ _ _) = ppr sty n
\end{code}

%************************************************************************
%*									*
\subsection[ClassOp-basic]{@ClassOp@: type and basic functions}
%*									*
%************************************************************************

\begin{code}
data ClassOp
  = MkClassOp	FAST_STRING -- The operation name

		Int	-- Unique within a class; starts at 1

		UniType	-- Type; the class tyvar is free (you can find
			-- it from the class). This means that a
			-- ClassOp doesn't make much sense outside the
			-- context of its parent class.
\end{code}

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
mkClassOp = MkClassOp

getClassOpTag :: ClassOp -> Int
getClassOpTag    (MkClassOp _ tag _) = tag

getClassOpString :: ClassOp -> FAST_STRING
getClassOpString (MkClassOp str _ _) = str

{- UNUSED:
getClassOpSig :: ClassOp -> ([TyVarTemplate], ThetaType, TauType)
getClassOpSig (MkClassOp _ _ ty) = splitType ty
-}

getClassOpLocalType :: ClassOp -> UniType {-SigmaType-}
getClassOpLocalType (MkClassOp _ _ ty) = ty
\end{code}

%************************************************************************
%*									*
\subsection[ClassOp-instances]{Instance declarations for @ClassOp@}
%*									*
%************************************************************************

@ClassOps@ are compared by their tags.

\begin{code}
instance Eq ClassOp where
    (MkClassOp _ i1 _) == (MkClassOp _ i2 _) = i1 == i2
    (MkClassOp _ i1 _) /= (MkClassOp _ i2 _) = i1 == i2

instance Ord ClassOp where
    (MkClassOp _ i1 _) <= (MkClassOp _ i2 _) = i1 <= i2
    (MkClassOp _ i1 _) <  (MkClassOp _ i2 _) = i1 <  i2
    (MkClassOp _ i1 _) >= (MkClassOp _ i2 _) = i1 >= i2
    (MkClassOp _ i1 _) >  (MkClassOp _ i2 _) = i1 >  i2
    -- ToDo: something for _tagCmp? (WDP 94/10)
\end{code}

And the usual output stuff:
\begin{code}
instance Outputable ClassOp where
    ppr = pprClassOp
\end{code}

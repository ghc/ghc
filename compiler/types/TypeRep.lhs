%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[TypeRep]{Type - friends' interface}

\begin{code}
-- We expose the relevant stuff from this module via the Type module
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TypeRep (
	TyThing(..), 
	Type(..),
	PredType(..),	 		-- to friends
	
 	Kind, ThetaType,		-- Synonyms

	funTyCon, funTyConName,

	-- Pretty-printing
	pprType, pprParendType, pprTypeApp,
	pprTyThing, pprTyThingCategory, 
	pprPred, pprEqPred, pprTheta, pprForAll, pprThetaArrow, pprClassPred,

	-- Kinds
	liftedTypeKind, unliftedTypeKind, openTypeKind,
        argTypeKind, ubxTupleKind,
	isLiftedTypeKindCon, isLiftedTypeKind,
	mkArrowKind, mkArrowKinds, isCoercionKind,
  	coVarPred,

        -- Kind constructors...
        liftedTypeKindTyCon, openTypeKindTyCon, unliftedTypeKindTyCon,
        argTypeKindTyCon, ubxTupleKindTyCon,

        -- And their names
        unliftedTypeKindTyConName, openTypeKindTyConName,
        ubxTupleKindTyConName, argTypeKindTyConName,
        liftedTypeKindTyConName,

        -- Super Kinds
	tySuperKind, coSuperKind,
        isTySuperKind, isCoSuperKind,
	tySuperKindTyCon, coSuperKindTyCon,
        
	pprKind, pprParendKind
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} DataCon( DataCon, dataConName )

-- friends:
import Var
import Name
import BasicTypes
import TyCon
import Class

-- others
import PrelNames
import Outputable
import FastString

-- libraries
import Data.Data hiding ( TyCon )
\end{code}

	----------------------
	A note about newtypes
	----------------------

Consider
	newtype N = MkN Int

Then we want N to be represented as an Int, and that's what we arrange.
The front end of the compiler [TcType.lhs] treats N as opaque, 
the back end treats it as transparent [Type.lhs].

There's a bit of a problem with recursive newtypes
	newtype P = MkP P
	newtype Q = MkQ (Q->Q)

Here the 'implicit expansion' we get from treating P and Q as transparent
would give rise to infinite types, which in turn makes eqType diverge.
Similarly splitForAllTys and splitFunTys can get into a loop.  

Solution: 

* Newtypes are always represented using TyConApp.

* For non-recursive newtypes, P, treat P just like a type synonym after 
  type-checking is done; i.e. it's opaque during type checking (functions
  from TcType) but transparent afterwards (functions from Type).  
  "Treat P as a type synonym" means "all functions expand NewTcApps 
  on the fly".

  Applications of the data constructor P simply vanish:
	P x = x
  

* For recursive newtypes Q, treat the Q and its representation as 
  distinct right through the compiler.  Applications of the data consructor
  use a coerce:
	Q = \(x::Q->Q). coerce Q x
  They are rare, so who cares if they are a tiny bit less efficient.

The typechecker (TcTyDecls) identifies enough type construtors as 'recursive'
to cut all loops.  The other members of the loop may be marked 'non-recursive'.


%************************************************************************
%*									*
\subsection{The data type}
%*									*
%************************************************************************


\begin{code}
-- | The key representation of types within the compiler
data Type
  = TyVarTy TyVar	-- ^ Vanilla type variable

  | AppTy
	Type
	Type		-- ^ Type application to something other than a 'TyCon'. Parameters:
	                --
	                --  1) Function: must /not/ be a 'TyConApp', must be another 'AppTy', or 'TyVarTy'
	                --
	                --  2) Argument type

  | TyConApp
	TyCon
	[Type]		-- ^ Application of a 'TyCon', including newtypes /and/ synonyms.
	                -- Invariant: saturated appliations of 'FunTyCon' must
	                -- use 'FunTy' and saturated synonyms must use their own
	                -- constructors. However, /unsaturated/ 'FunTyCon's do appear as 'TyConApp's.
	                -- Parameters:
	                --
	                -- 1) Type constructor being applied to.
	                --
	                -- 2) Type arguments. Might not have enough type arguments here to saturate the constructor.
	                -- Even type synonyms are not necessarily saturated; for example unsaturated type synonyms
	                -- can appear as the right hand side of a type synonym.

  | FunTy
	Type
	Type		-- ^ Special case of 'TyConApp': @TyConApp FunTyCon [t1, t2]@

  | ForAllTy
	TyVar
	Type	        -- ^ A polymorphic type

  | PredTy
	PredType	-- ^ The type of evidence for a type predictate.
                        -- Note that a @PredTy (EqPred _ _)@ can appear only as the kind
	                -- of a coercion variable; never as the argument or result
	                -- of a 'FunTy' (unlike the 'PredType' constructors 'ClassP' or 'IParam')
	                
	                -- See Note [PredTy], and Note [Equality predicates]
  deriving (Data, Typeable)

-- | The key type representing kinds in the compiler.
-- Invariant: a kind is always in one of these forms:
--
-- > FunTy k1 k2
-- > TyConApp PrimTyCon [...]
-- > TyVar kv   -- (during inference only)
-- > ForAll ... -- (for top-level coercions)
type Kind = Type

-- | "Super kinds", used to help encode 'Kind's as types.
-- Invariant: a super kind is always of this form:
--
-- > TyConApp SuperKindTyCon ...
type SuperKind = Type
\end{code}

-------------------------------------
 		Note [PredTy]

\begin{code}
-- | A type of the form @PredTy p@ represents a value whose type is
-- the Haskell predicate @p@, where a predicate is what occurs before 
-- the @=>@ in a Haskell type.
-- It can be expanded into its representation, but: 
--
-- * The type checker must treat it as opaque
--
-- * The rest of the compiler treats it as transparent
--
-- Consider these examples:
--
-- > f :: (Eq a) => a -> Int
-- > g :: (?x :: Int -> Int) => a -> Int
-- > h :: (r\l) => {r} => {l::Int | r}
--
-- Here the @Eq a@ and @?x :: Int -> Int@ and @r\l@ are all called \"predicates\"
data PredType 
  = ClassP Class [Type]		-- ^ Class predicate e.g. @Eq a@
  | IParam (IPName Name) Type	-- ^ Implicit parameter e.g. @?x :: Int@
  | EqPred Type Type		-- ^ Equality predicate e.g @ty1 ~ ty2@
  deriving (Data, Typeable)

-- | A collection of 'PredType's
type ThetaType = [PredType]
\end{code}

(We don't support TREX records yet, but the setup is designed
to expand to allow them.)

A Haskell qualified type, such as that for f,g,h above, is
represented using 
	* a FunTy for the double arrow
	* with a PredTy as the function argument

The predicate really does turn into a real extra argument to the
function.  If the argument has type (PredTy p) then the predicate p is
represented by evidence (a dictionary, for example, of type (predRepTy p).

Note [Equality predicates]
~~~~~~~~~~~~~~~~~~~~~~~~~~
	forall a b. (a ~ S b) => a -> b
could be represented by
	ForAllTy a (ForAllTy b (FunTy (PredTy (EqPred a (S b))) ...))
OR
	ForAllTy a (ForAllTy b (ForAllTy (c::PredTy (EqPred a (S b))) ...))

The latter is what we do.  (Unlike for class and implicit parameter
constraints, which do use FunTy.)

Reason:
	* FunTy is always a *value* function
	* ForAllTy is discarded at runtime

We often need to make a "wildcard" (c::PredTy..).  We always use the same
name (wildCoVarName), since it's not mentioned.


%************************************************************************
%*									*
			TyThing
%*									*
%************************************************************************

Despite the fact that DataCon has to be imported via a hi-boot route, 
this module seems the right place for TyThing, because it's needed for
funTyCon and all the types in TysPrim.

\begin{code}
-- | A typecheckable-thing, essentially anything that has a name
data TyThing = AnId     Id
	     | ADataCon DataCon
	     | ATyCon   TyCon
	     | AClass   Class

instance Outputable TyThing where 
  ppr = pprTyThing

pprTyThing :: TyThing -> SDoc
pprTyThing thing = pprTyThingCategory thing <+> quotes (ppr (getName thing))

pprTyThingCategory :: TyThing -> SDoc
pprTyThingCategory (ATyCon _) 	= ptext (sLit "Type constructor")
pprTyThingCategory (AClass _)   = ptext (sLit "Class")
pprTyThingCategory (AnId   _)   = ptext (sLit "Identifier")
pprTyThingCategory (ADataCon _) = ptext (sLit "Data constructor")

instance NamedThing TyThing where	-- Can't put this with the type
  getName (AnId id)     = getName id	-- decl, because the DataCon instance
  getName (ATyCon tc)   = getName tc	-- isn't visible there
  getName (AClass cl)   = getName cl
  getName (ADataCon dc) = dataConName dc
\end{code}


%************************************************************************
%*									*
		Wired-in type constructors
%*									*
%************************************************************************

We define a few wired-in type constructors here to avoid module knots

\begin{code}
--------------------------
-- First the TyCons...

-- | See "Type#kind_subtyping" for details of the distinction between the 'Kind' 'TyCon's
funTyCon, tySuperKindTyCon, coSuperKindTyCon, liftedTypeKindTyCon,
      openTypeKindTyCon, unliftedTypeKindTyCon,
      ubxTupleKindTyCon, argTypeKindTyCon
   :: TyCon
funTyConName, tySuperKindTyConName, coSuperKindTyConName, liftedTypeKindTyConName,
      openTypeKindTyConName, unliftedTypeKindTyConName,
      ubxTupleKindTyConName, argTypeKindTyConName
   :: Name

funTyCon = mkFunTyCon funTyConName (mkArrowKinds [argTypeKind, openTypeKind] liftedTypeKind)
	-- You might think that (->) should have type (?? -> ? -> *), and you'd be right
	-- But if we do that we get kind errors when saying
	--	instance Control.Arrow (->)
	-- becuase the expected kind is (*->*->*).  The trouble is that the
	-- expected/actual stuff in the unifier does not go contra-variant, whereas
	-- the kind sub-typing does.  Sigh.  It really only matters if you use (->) in
	-- a prefix way, thus:  (->) Int# Int#.  And this is unusual.


tySuperKindTyCon     = mkSuperKindTyCon tySuperKindTyConName
coSuperKindTyCon     = mkSuperKindTyCon coSuperKindTyConName

liftedTypeKindTyCon   = mkKindTyCon liftedTypeKindTyConName   tySuperKind
openTypeKindTyCon     = mkKindTyCon openTypeKindTyConName     tySuperKind
unliftedTypeKindTyCon = mkKindTyCon unliftedTypeKindTyConName tySuperKind
ubxTupleKindTyCon     = mkKindTyCon ubxTupleKindTyConName     tySuperKind
argTypeKindTyCon      = mkKindTyCon argTypeKindTyConName      tySuperKind

--------------------------
-- ... and now their names

tySuperKindTyConName      = mkPrimTyConName (fsLit "BOX") tySuperKindTyConKey tySuperKindTyCon
coSuperKindTyConName      = mkPrimTyConName (fsLit "COERCION") coSuperKindTyConKey coSuperKindTyCon
liftedTypeKindTyConName   = mkPrimTyConName (fsLit "*") liftedTypeKindTyConKey liftedTypeKindTyCon
openTypeKindTyConName     = mkPrimTyConName (fsLit "?") openTypeKindTyConKey openTypeKindTyCon
unliftedTypeKindTyConName = mkPrimTyConName (fsLit "#") unliftedTypeKindTyConKey unliftedTypeKindTyCon
ubxTupleKindTyConName     = mkPrimTyConName (fsLit "(#)") ubxTupleKindTyConKey ubxTupleKindTyCon
argTypeKindTyConName      = mkPrimTyConName (fsLit "??") argTypeKindTyConKey argTypeKindTyCon
funTyConName              = mkPrimTyConName (fsLit "(->)") funTyConKey funTyCon

mkPrimTyConName :: FastString -> Unique -> TyCon -> Name
mkPrimTyConName occ key tycon = mkWiredInName gHC_PRIM (mkTcOccFS occ) 
					      key 
					      (ATyCon tycon)
					      BuiltInSyntax
	-- All of the super kinds and kinds are defined in Prim and use BuiltInSyntax,
	-- because they are never in scope in the source

------------------
-- We also need Kinds and SuperKinds, locally and in TyCon

kindTyConType :: TyCon -> Type
kindTyConType kind = TyConApp kind []

-- | See "Type#kind_subtyping" for details of the distinction between these 'Kind's
liftedTypeKind, unliftedTypeKind, openTypeKind, argTypeKind, ubxTupleKind :: Kind

liftedTypeKind   = kindTyConType liftedTypeKindTyCon
unliftedTypeKind = kindTyConType unliftedTypeKindTyCon
openTypeKind     = kindTyConType openTypeKindTyCon
argTypeKind      = kindTyConType argTypeKindTyCon
ubxTupleKind	 = kindTyConType ubxTupleKindTyCon

-- | Given two kinds @k1@ and @k2@, creates the 'Kind' @k1 -> k2@
mkArrowKind :: Kind -> Kind -> Kind
mkArrowKind k1 k2 = FunTy k1 k2

-- | Iterated application of 'mkArrowKind'
mkArrowKinds :: [Kind] -> Kind -> Kind
mkArrowKinds arg_kinds result_kind = foldr mkArrowKind result_kind arg_kinds

tySuperKind, coSuperKind :: SuperKind
tySuperKind = kindTyConType tySuperKindTyCon 
coSuperKind = kindTyConType coSuperKindTyCon 

isTySuperKind :: SuperKind -> Bool
isTySuperKind (TyConApp kc []) = kc `hasKey` tySuperKindTyConKey
isTySuperKind _                = False

isCoSuperKind :: SuperKind -> Bool
isCoSuperKind (TyConApp kc []) = kc `hasKey` coSuperKindTyConKey
isCoSuperKind _                = False

-------------------
-- Lastly we need a few functions on Kinds

isLiftedTypeKindCon :: TyCon -> Bool
isLiftedTypeKindCon tc    = tc `hasKey` liftedTypeKindTyConKey

isLiftedTypeKind :: Kind -> Bool
isLiftedTypeKind (TyConApp tc []) = isLiftedTypeKindCon tc
isLiftedTypeKind _                = False

isCoercionKind :: Kind -> Bool
-- All coercions are of form (ty1 ~ ty2)
-- This function is here rather than in Coercion, 
-- because it's used in a knot-tied way to enforce invariants in Var
isCoercionKind (PredTy (EqPred {})) = True
isCoercionKind _                    = False

coVarPred :: CoVar -> PredType
coVarPred tv
  = ASSERT( isCoVar tv )
    case tyVarKind tv of
	PredTy eq -> eq
	other	  -> pprPanic "coVarPred" (ppr tv $$ ppr other)
\end{code}



%************************************************************************
%*									*
\subsection{The external interface}
%*									*
%************************************************************************

@pprType@ is the standard @Type@ printer; the overloaded @ppr@ function is
defined to use this.  @pprParendType@ is the same, except it puts
parens around the type, except for the atomic cases.  @pprParendType@
works just by setting the initial context precedence very high.

\begin{code}
data Prec = TopPrec 	-- No parens
	  | FunPrec 	-- Function args; no parens for tycon apps
	  | TyConPrec 	-- Tycon args; no parens for atomic
	  deriving( Eq, Ord )

maybeParen :: Prec -> Prec -> SDoc -> SDoc
maybeParen ctxt_prec inner_prec pretty
  | ctxt_prec < inner_prec = pretty
  | otherwise		   = parens pretty

------------------
pprType, pprParendType :: Type -> SDoc
pprType       ty = ppr_type TopPrec   ty
pprParendType ty = ppr_type TyConPrec ty

pprTypeApp :: NamedThing a => a -> [Type] -> SDoc
-- The first arg is the tycon, or sometimes class
-- Print infix if the tycon/class looks like an operator
pprTypeApp tc tys = ppr_type_app TopPrec (getName tc) tys

------------------
pprPred :: PredType -> SDoc
pprPred (ClassP cls tys) = pprClassPred cls tys
pprPred (IParam ip ty)   = ppr ip <> dcolon <> pprType ty
pprPred (EqPred ty1 ty2) = pprEqPred (ty1,ty2)

pprEqPred :: (Type,Type) -> SDoc
pprEqPred (ty1,ty2) = sep [ ppr_type FunPrec ty1
                          , nest 2 (ptext (sLit "~"))
                          , ppr_type FunPrec ty2]
			       -- Precedence looks like (->) so that we get
			       --    Maybe a ~ Bool
			       --    (a->a) ~ Bool
			       -- Note parens on the latter!

pprClassPred :: Class -> [Type] -> SDoc
pprClassPred clas tys = ppr_type_app TopPrec (getName clas) tys

pprTheta :: ThetaType -> SDoc
-- pprTheta [pred] = pprPred pred	 -- I'm in two minds about this
pprTheta theta  = parens (sep (punctuate comma (map pprPred theta)))

pprThetaArrow :: ThetaType -> SDoc
pprThetaArrow []     = empty
pprThetaArrow [pred] 
  | noParenPred pred = pprPred pred <+> darrow
pprThetaArrow preds  = parens (sep (punctuate comma (map pprPred preds))) <+> darrow

noParenPred :: PredType -> Bool
-- A predicate that can appear without parens before a "=>"
--       C a => a -> a
--       a~b => a -> b
-- But   (?x::Int) => Int -> Int
noParenPred (ClassP {}) = True
noParenPred (EqPred {}) = True
noParenPred (IParam {}) = False

------------------
instance Outputable Type where
    ppr ty = pprType ty

instance Outputable PredType where
    ppr = pprPred

instance Outputable name => OutputableBndr (IPName name) where
    pprBndr _ n = ppr n	-- Simple for now

------------------
	-- OK, here's the main printer

pprKind, pprParendKind :: Kind -> SDoc
pprKind = pprType
pprParendKind = pprParendType

ppr_type :: Prec -> Type -> SDoc
ppr_type _ (TyVarTy tv)		-- Note [Infix type variables]
  | isSymOcc (getOccName tv)  = parens (ppr tv)
  | otherwise		      = ppr tv
ppr_type p (PredTy pred)      = maybeParen p TyConPrec $
                                ifPprDebug (ptext (sLit "<pred>")) <> (ppr pred)
ppr_type p (TyConApp tc tys)  = ppr_tc_app p tc tys

ppr_type p (AppTy t1 t2) = maybeParen p TyConPrec $
			   pprType t1 <+> ppr_type TyConPrec t2

ppr_type p ty@(ForAllTy _ _)       = ppr_forall_type p ty
ppr_type p ty@(FunTy (PredTy _) _) = ppr_forall_type p ty

ppr_type p (FunTy ty1 ty2)
  = -- We don't want to lose synonyms, so we mustn't use splitFunTys here.
    maybeParen p FunPrec $
    sep (ppr_type FunPrec ty1 : ppr_fun_tail ty2)
  where
    ppr_fun_tail (FunTy ty1 ty2) 
      | not (is_pred ty1) = (arrow <+> ppr_type FunPrec ty1) : ppr_fun_tail ty2
    ppr_fun_tail other_ty = [arrow <+> pprType other_ty]
    is_pred (PredTy {}) = True
    is_pred _           = False

ppr_forall_type :: Prec -> Type -> SDoc
ppr_forall_type p ty
  = maybeParen p FunPrec $
    sep [pprForAll tvs, pprThetaArrow ctxt, pprType tau]
  where
    (tvs,  rho) = split1 [] ty
    (ctxt, tau) = split2 [] rho

    -- We need to be extra careful here as equality constraints will occur as
    -- type variables with an equality kind.  So, while collecting quantified
    -- variables, we separate the coercion variables out and turn them into
    -- equality predicates.
    split1 tvs (ForAllTy tv ty) 
      | not (isCoVar tv)     = split1 (tv:tvs) ty
    split1 tvs ty	     = (reverse tvs, ty)
 
    split2 ps (PredTy p `FunTy` ty) = split2 (p:ps) ty
    split2 ps (ForAllTy tv ty) 
	| isCoVar tv		    = split2 (coVarPred tv : ps) ty
    split2 ps ty		    = (reverse ps, ty)

ppr_tc_app :: Prec -> TyCon -> [Type] -> SDoc
ppr_tc_app _ tc []
  = ppr_tc tc
ppr_tc_app _ tc [ty]
  | tc `hasKey` listTyConKey = brackets (pprType ty)
  | tc `hasKey` parrTyConKey = ptext (sLit "[:") <> pprType ty <> ptext (sLit ":]")
  | tc `hasKey` liftedTypeKindTyConKey   = ptext (sLit "*")
  | tc `hasKey` unliftedTypeKindTyConKey = ptext (sLit "#")
  | tc `hasKey` openTypeKindTyConKey     = ptext (sLit "(?)")
  | tc `hasKey` ubxTupleKindTyConKey     = ptext (sLit "(#)")
  | tc `hasKey` argTypeKindTyConKey      = ptext (sLit "??")

ppr_tc_app p tc tys
  | isTupleTyCon tc && tyConArity tc == length tys
  = tupleParens (tupleTyConBoxity tc) (sep (punctuate comma (map pprType tys)))
  | otherwise
  = ppr_type_app p (getName tc) tys

ppr_type_app :: Prec -> Name -> [Type] -> SDoc
-- Used for classes as well as types; that's why it's separate from ppr_tc_app
ppr_type_app p tc tys
  | is_sym_occ		-- Print infix if possible
  , [ty1,ty2] <- tys	-- We know nothing of precedence though
  = maybeParen p FunPrec (sep [ppr_type FunPrec ty1, 
			       pprInfixVar True (ppr tc) <+> ppr_type FunPrec ty2])
  | otherwise
  = maybeParen p TyConPrec (hang (pprPrefixVar is_sym_occ (ppr tc))
    	       	 	       2 (sep (map pprParendType tys)))
  where
    is_sym_occ = isSymOcc (getOccName tc)

ppr_tc :: TyCon -> SDoc	-- No brackets for SymOcc
ppr_tc tc 
  = pp_nt_debug <> ppr tc
  where
   pp_nt_debug | isNewTyCon tc = ifPprDebug (if isRecursiveTyCon tc 
				             then ptext (sLit "<recnt>")
					     else ptext (sLit "<nt>"))
	       | otherwise     = empty

-------------------
pprForAll :: [TyVar] -> SDoc
pprForAll []  = empty
pprForAll tvs = ptext (sLit "forall") <+> sep (map pprTvBndr tvs) <> dot

pprTvBndr :: TyVar -> SDoc
pprTvBndr tv | isLiftedTypeKind kind = ppr tv
	     | otherwise	     = parens (ppr tv <+> dcolon <+> pprKind kind)
	     where
	       kind = tyVarKind tv
\end{code}

Note [Infix type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
In Haskell 98 you can say

   f :: (a ~> b) -> b

and the (~>) is considered a type variable.  However, the type
pretty-printer in this module will just see (a ~> b) as

   App (App (TyVarTy "~>") (TyVarTy "a")) (TyVarTy "b")

So it'll print the type in prefix form.  To avoid confusion we must
remember to parenthesise the operator, thus

   (~>) a b -> b

See Trac #2766.





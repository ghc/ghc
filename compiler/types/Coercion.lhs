%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | Module for (a) type kinds and (b) type coercions, 
-- as used in System FC. See 'CoreSyn.Expr' for
-- more on System FC and how coercions fit into it.
--
module Coercion (
        -- * Main data type
        Coercion(..), Var, CoVar,
        LeftOrRight(..), pickLR,

        -- ** Functions over coercions
        coVarKind,
        coercionType, coercionKind, coercionKinds, isReflCo,
        isReflCo_maybe,
        mkCoercionType,

	-- ** Constructing coercions
        mkReflCo, mkCoVarCo, 
        mkAxInstCo, mkUnbranchedAxInstCo, mkAxInstLHS, mkAxInstRHS,
        mkUnbranchedAxInstRHS,
        mkPiCo, mkPiCos, mkCoCast,
        mkSymCo, mkTransCo, mkNthCo, mkLRCo,
	mkInstCo, mkAppCo, mkTyConAppCo, mkFunCo,
        mkForAllCo, mkUnsafeCo,
        mkNewTypeCo, 

        -- ** Decomposition
        splitNewTypeRepCo_maybe, instNewTyCon_maybe, 
        topNormaliseNewType, topNormaliseNewTypeX,

        decomposeCo, getCoVar_maybe,
        splitTyConAppCo_maybe,
        splitAppCo_maybe,
        splitForAllCo_maybe,

	-- ** Coercion variables
	mkCoVar, isCoVar, isCoVarType, coVarName, setCoVarName, setCoVarUnique,

        -- ** Free variables
        tyCoVarsOfCo, tyCoVarsOfCos, coVarsOfCo, coercionSize,
	
        -- ** Substitution
        CvSubstEnv, emptyCvSubstEnv, 
 	CvSubst(..), emptyCvSubst, Coercion.lookupTyVar, lookupCoVar,
	isEmptyCvSubst, zapCvSubstEnv, getCvInScope,
        substCo, substCos, substCoVar, substCoVars,
        substCoWithTy, substCoWithTys, 
	cvTvSubst, tvCvSubst, mkCvSubst, zipOpenCvSubst,
        substTy, extendTvSubst, extendCvSubstAndInScope,
	substTyVarBndr, substCoVarBndr,

	-- ** Lifting
	liftCoMatch, liftCoSubstTyVar, liftCoSubstWith, 
        
        -- ** Comparison
        coreEqCoercion, coreEqCoercion2,

        -- ** Forcing evaluation of coercions
        seqCo,
        
        -- * Pretty-printing
        pprCo, pprParendCo, 
        pprCoAxiom, pprCoAxBranch, pprCoAxBranchHdr, 

        -- * Tidying
        tidyCo, tidyCos,

        -- * Other
        applyCo
       ) where 

#include "HsVersions.h"

import Unify	( MatchEnv(..), matchList )
import TypeRep
import qualified Type
import Type hiding( substTy, substTyVarBndr, extendTvSubst )
import TyCon
import CoAxiom
import Var
import VarEnv
import VarSet
import Binary
import Maybes   ( orElse )
import Name	( Name, NamedThing(..), nameUnique, nameModule, getSrcSpan )
import OccName 	( parenSymOcc )
import Util
import BasicTypes
import Outputable
import Unique
import Pair
import SrcLoc
import PrelNames	( funTyConKey, eqPrimTyConKey )
import Control.Applicative
import Data.Traversable (traverse, sequenceA)
import Control.Arrow (second)
import FastString

import qualified Data.Data as Data hiding ( TyCon )
\end{code}

%************************************************************************
%*									*
            Coercions
%*									*
%************************************************************************

\begin{code}
-- | A 'Coercion' is concrete evidence of the equality/convertibility
-- of two types.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data Coercion 
  -- These ones mirror the shape of types
  = Refl Type  -- See Note [Refl invariant]
          -- Invariant: applications of (Refl T) to a bunch of identity coercions
          --            always show up as Refl.
          -- For example  (Refl T) (Refl a) (Refl b) shows up as (Refl (T a b)).

          -- Applications of (Refl T) to some coercions, at least one of
          -- which is NOT the identity, show up as TyConAppCo.
          -- (They may not be fully saturated however.)
          -- ConAppCo coercions (like all coercions other than Refl)
          -- are NEVER the identity.

  -- These ones simply lift the correspondingly-named 
  -- Type constructors into Coercions
  | TyConAppCo TyCon [Coercion]    -- lift TyConApp 
    	       -- The TyCon is never a synonym; 
	       -- we expand synonyms eagerly
	       -- But it can be a type function

  | AppCo Coercion Coercion        -- lift AppTy

  -- See Note [Forall coercions]
  | ForAllCo TyVar Coercion       -- forall a. g

  -- These are special
  | CoVarCo CoVar
  | AxiomInstCo (CoAxiom Branched) BranchIndex [Coercion]
     -- See also [CoAxiom index]
     -- The coercion arguments always *precisely* saturate 
     -- arity of (that branch of) the CoAxiom.  If there are
     -- any left over, we use AppCo.  See 
     -- See [Coercion axioms applied to coercions]

  | UnsafeCo Type Type
  | SymCo Coercion
  | TransCo Coercion Coercion

  -- These are destructors
  | NthCo  Int         Coercion     -- Zero-indexed; decomposes (T t0 ... tn)
  | LRCo   LeftOrRight Coercion     -- Decomposes (t_left t_right)
  | InstCo Coercion Type
  deriving (Data.Data, Data.Typeable)

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.lhs
data LeftOrRight = CLeft | CRight 
                 deriving( Eq, Data.Data, Data.Typeable )

instance Binary LeftOrRight where
   put_ bh CLeft  = putByte bh 0
   put_ bh CRight = putByte bh 1

   get bh = do { h <- getByte bh
               ; case h of
                   0 -> return CLeft
                   _ -> return CRight }

pickLR :: LeftOrRight -> (a,a) -> a
pickLR CLeft  (l,_) = l
pickLR CRight (_,r) = r
\end{code}


Note [Refl invariant]
~~~~~~~~~~~~~~~~~~~~~
Coercions have the following invariant 
     Refl is always lifted as far as possible.  

You might think that a consequencs is:
     Every identity coercions has Refl at the root

But that's not quite true because of coercion variables.  Consider
     g         where g :: Int~Int
     Left h    where h :: Maybe Int ~ Maybe Int
etc.  So the consequence is only true of coercions that
have no coercion variables.

Note [Coercion axioms applied to coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The reason coercion axioms can be applied to coercions and not just
types is to allow for better optimization.  There are some cases where
we need to be able to "push transitivity inside" an axiom in order to
expose further opportunities for optimization.  

For example, suppose we have

  C a : t[a] ~ F a
  g   : b ~ c

and we want to optimize

  sym (C b) ; t[g] ; C c

which has the kind

  F b ~ F c

(stopping through t[b] and t[c] along the way).

We'd like to optimize this to just F g -- but how?  The key is
that we need to allow axioms to be instantiated by *coercions*,
not just by types.  Then we can (in certain cases) push
transitivity inside the axiom instantiations, and then react
opposite-polarity instantiations of the same axiom.  In this
case, e.g., we match t[g] against the LHS of (C c)'s kind, to
obtain the substitution  a |-> g  (note this operation is sort
of the dual of lifting!) and hence end up with

  C g : t[b] ~ F c

which indeed has the same kind as  t[g] ; C c.

Now we have

  sym (C b) ; C g

which can be optimized to F g.

Note [CoAxiom index]
~~~~~~~~~~~~~~~~~~~~
A CoAxiom has 1 or more branches. Each branch has contains a list
of the free type variables in that branch, the LHS type patterns,
and the RHS type for that branch. When we apply an axiom to a list
of coercions, we must choose which branch of the axiom we wish to
use, as the different branches may have different numbers of free
type variables. (The number of type patterns is always the same
among branches, but that doesn't quite concern us here.)

The Int in the AxiomInstCo constructor is the 0-indexed number
of the chosen branch.

Note [Forall coercions]
~~~~~~~~~~~~~~~~~~~~~~~
Constructing coercions between forall-types can be a bit tricky.
Currently, the situation is as follows:

  ForAllCo TyVar Coercion

represents a coercion between polymorphic types, with the rule

           v : k       g : t1 ~ t2
  ----------------------------------------------
  ForAllCo v g : (all v:k . t1) ~ (all v:k . t2)

Note that it's only necessary to coerce between polymorphic types
where the type variables have identical kinds, because equality on
kinds is trivial.

Note [Predicate coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   g :: a~b
How can we coerce between types
   ([c]~a) => [a] -> c
and
   ([c]~b) => [b] -> c
where the equality predicate *itself* differs?

Answer: we simply treat (~) as an ordinary type constructor, so these
types really look like

   ((~) [c] a) -> [a] -> c
   ((~) [c] b) -> [b] -> c

So the coercion between the two is obviously

   ((~) [c] g) -> [g] -> c

Another way to see this to say that we simply collapse predicates to
their representation type (see Type.coreView and Type.predTypeRep).

This collapse is done by mkPredCo; there is no PredCo constructor
in Coercion.  This is important because we need Nth to work on 
predicates too:
    Nth 1 ((~) [c] g) = g
See Simplify.simplCoercionF, which generates such selections.

Note [Kind coercions]
~~~~~~~~~~~~~~~~~~~~~
Suppose T :: * -> *, and g :: A ~ B
Then the coercion
   TyConAppCo T [g]      T g : T A ~ T B

Now suppose S :: forall k. k -> *, and g :: A ~ B
Then the coercion
   TyConAppCo S [Refl *, g]   T <*> g : T * A ~ T * B

Notice that the arguments to TyConAppCo are coercions, but the first
represents a *kind* coercion. Now, we don't allow any non-trivial kind
coercions, so it's an invariant that any such kind coercions are Refl.
Lint checks this. 

However it's inconvenient to insist that these kind coercions are always
*structurally* (Refl k), because the key function exprIsConApp_maybe
pushes coercions into constructor arguments, so 
       C k ty e |> g
may turn into
       C (Nth 0 g) ....
Now (Nth 0 g) will optimise to Refl, but perhaps not instantly.

%************************************************************************
%*									*
\subsection{Coercion variables}
%*									*
%************************************************************************

\begin{code}
coVarName :: CoVar -> Name
coVarName = varName

setCoVarUnique :: CoVar -> Unique -> CoVar
setCoVarUnique = setVarUnique

setCoVarName :: CoVar -> Name -> CoVar
setCoVarName   = setVarName

isCoVar :: Var -> Bool
isCoVar v = isCoVarType (varType v)

isCoVarType :: Type -> Bool
isCoVarType ty 	    -- Tests for t1 ~# t2, the unboxed equality
  = case splitTyConApp_maybe ty of
      Just (tc,tys) -> tc `hasKey` eqPrimTyConKey && tys `lengthAtLeast` 2
      Nothing       -> False
\end{code}


\begin{code}
tyCoVarsOfCo :: Coercion -> VarSet
-- Extracts type and coercion variables from a coercion
tyCoVarsOfCo (Refl ty)           = tyVarsOfType ty
tyCoVarsOfCo (TyConAppCo _ cos)  = tyCoVarsOfCos cos
tyCoVarsOfCo (AppCo co1 co2)     = tyCoVarsOfCo co1 `unionVarSet` tyCoVarsOfCo co2
tyCoVarsOfCo (ForAllCo tv co)    = tyCoVarsOfCo co `delVarSet` tv
tyCoVarsOfCo (CoVarCo v)         = unitVarSet v
tyCoVarsOfCo (AxiomInstCo _ _ cos) = tyCoVarsOfCos cos
tyCoVarsOfCo (UnsafeCo ty1 ty2)  = tyVarsOfType ty1 `unionVarSet` tyVarsOfType ty2
tyCoVarsOfCo (SymCo co)          = tyCoVarsOfCo co
tyCoVarsOfCo (TransCo co1 co2)   = tyCoVarsOfCo co1 `unionVarSet` tyCoVarsOfCo co2
tyCoVarsOfCo (NthCo _ co)        = tyCoVarsOfCo co
tyCoVarsOfCo (LRCo _ co)         = tyCoVarsOfCo co
tyCoVarsOfCo (InstCo co ty)      = tyCoVarsOfCo co `unionVarSet` tyVarsOfType ty

tyCoVarsOfCos :: [Coercion] -> VarSet
tyCoVarsOfCos cos = foldr (unionVarSet . tyCoVarsOfCo) emptyVarSet cos

coVarsOfCo :: Coercion -> VarSet
-- Extract *coerction* variables only.  Tiresome to repeat the code, but easy.
coVarsOfCo (Refl _)            = emptyVarSet
coVarsOfCo (TyConAppCo _ cos)  = coVarsOfCos cos
coVarsOfCo (AppCo co1 co2)     = coVarsOfCo co1 `unionVarSet` coVarsOfCo co2
coVarsOfCo (ForAllCo _ co)     = coVarsOfCo co
coVarsOfCo (CoVarCo v)         = unitVarSet v
coVarsOfCo (AxiomInstCo _ _ cos) = coVarsOfCos cos
coVarsOfCo (UnsafeCo _ _)      = emptyVarSet
coVarsOfCo (SymCo co)          = coVarsOfCo co
coVarsOfCo (TransCo co1 co2)   = coVarsOfCo co1 `unionVarSet` coVarsOfCo co2
coVarsOfCo (NthCo _ co)        = coVarsOfCo co
coVarsOfCo (LRCo _ co)         = coVarsOfCo co
coVarsOfCo (InstCo co _)       = coVarsOfCo co

coVarsOfCos :: [Coercion] -> VarSet
coVarsOfCos cos = foldr (unionVarSet . coVarsOfCo) emptyVarSet cos

coercionSize :: Coercion -> Int
coercionSize (Refl ty)           = typeSize ty
coercionSize (TyConAppCo _ cos)  = 1 + sum (map coercionSize cos)
coercionSize (AppCo co1 co2)     = coercionSize co1 + coercionSize co2
coercionSize (ForAllCo _ co)     = 1 + coercionSize co
coercionSize (CoVarCo _)         = 1
coercionSize (AxiomInstCo _ _ cos) = 1 + sum (map coercionSize cos)
coercionSize (UnsafeCo ty1 ty2)  = typeSize ty1 + typeSize ty2
coercionSize (SymCo co)          = 1 + coercionSize co
coercionSize (TransCo co1 co2)   = 1 + coercionSize co1 + coercionSize co2
coercionSize (NthCo _ co)        = 1 + coercionSize co
coercionSize (LRCo  _ co)        = 1 + coercionSize co
coercionSize (InstCo co ty)      = 1 + coercionSize co + typeSize ty
\end{code}

%************************************************************************
%*									*
                            Tidying coercions
%*									*
%************************************************************************

\begin{code}
tidyCo :: TidyEnv -> Coercion -> Coercion
tidyCo env@(_, subst) co
  = go co
  where
    go (Refl ty)             = Refl (tidyType env ty)
    go (TyConAppCo tc cos)   = let args = map go cos
                               in args `seqList` TyConAppCo tc args
    go (AppCo co1 co2)       = (AppCo $! go co1) $! go co2
    go (ForAllCo tv co)      = ForAllCo tvp $! (tidyCo envp co)
                               where
                                 (envp, tvp) = tidyTyVarBndr env tv
    go (CoVarCo cv)          = case lookupVarEnv subst cv of
                                 Nothing  -> CoVarCo cv
                                 Just cv' -> CoVarCo cv'
    go (AxiomInstCo con ind cos) = let args = tidyCos env cos
                               in  args `seqList` AxiomInstCo con ind args
    go (UnsafeCo ty1 ty2)    = (UnsafeCo $! tidyType env ty1) $! tidyType env ty2
    go (SymCo co)            = SymCo $! go co
    go (TransCo co1 co2)     = (TransCo $! go co1) $! go co2
    go (NthCo d co)          = NthCo d $! go co
    go (LRCo lr co)          = LRCo lr $! go co
    go (InstCo co ty)        = (InstCo $! go co) $! tidyType env ty

tidyCos :: TidyEnv -> [Coercion] -> [Coercion]
tidyCos env = map (tidyCo env)
\end{code}

%************************************************************************
%*									*
                   Pretty-printing coercions
%*                                                                      *
%************************************************************************

@pprCo@ is the standard @Coercion@ printer; the overloaded @ppr@
function is defined to use this.  @pprParendCo@ is the same, except it
puts parens around the type, except for the atomic cases.
@pprParendCo@ works just by setting the initial context precedence
very high.

\begin{code}
instance Outputable Coercion where
  ppr = pprCo

pprCo, pprParendCo :: Coercion -> SDoc
pprCo       co = ppr_co TopPrec   co
pprParendCo co = ppr_co TyConPrec co

ppr_co :: Prec -> Coercion -> SDoc
ppr_co _ (Refl ty) = angleBrackets (ppr ty)

ppr_co p co@(TyConAppCo tc [_,_])
  | tc `hasKey` funTyConKey = ppr_fun_co p co

ppr_co p (TyConAppCo tc cos)   = pprTcApp   p ppr_co tc cos
ppr_co p (AppCo co1 co2)       = maybeParen p TyConPrec $
                                 pprCo co1 <+> ppr_co TyConPrec co2
ppr_co p co@(ForAllCo {})      = ppr_forall_co p co
ppr_co _ (CoVarCo cv)          = parenSymOcc (getOccName cv) (ppr cv)
ppr_co p (AxiomInstCo con index cos)
  = pprPrefixApp p (ppr (getName con) <> brackets (ppr index))
                   (map (ppr_co TyConPrec) cos)

ppr_co p co@(TransCo {}) = maybeParen p FunPrec $
                           case trans_co_list co [] of
                             [] -> panic "ppr_co"
                             (co:cos) -> sep ( ppr_co FunPrec co
                                             : [ char ';' <+> ppr_co FunPrec co | co <- cos])
ppr_co p (InstCo co ty) = maybeParen p TyConPrec $
                          pprParendCo co <> ptext (sLit "@") <> pprType ty

ppr_co p (UnsafeCo ty1 ty2) = pprPrefixApp p (ptext (sLit "UnsafeCo")) 
                                           [pprParendType ty1, pprParendType ty2]
ppr_co p (SymCo co)         = pprPrefixApp p (ptext (sLit "Sym")) [pprParendCo co]
ppr_co p (NthCo n co)       = pprPrefixApp p (ptext (sLit "Nth:") <> int n) [pprParendCo co]
ppr_co p (LRCo sel co)      = pprPrefixApp p (ppr sel) [pprParendCo co]

trans_co_list :: Coercion -> [Coercion] -> [Coercion]
trans_co_list (TransCo co1 co2) cos = trans_co_list co1 (trans_co_list co2 cos)
trans_co_list co                cos = co : cos

instance Outputable LeftOrRight where
  ppr CLeft    = ptext (sLit "Left")
  ppr CRight   = ptext (sLit "Right")

ppr_fun_co :: Prec -> Coercion -> SDoc
ppr_fun_co p co = pprArrowChain p (split co)
  where
    split :: Coercion -> [SDoc]
    split (TyConAppCo f [arg,res])
      | f `hasKey` funTyConKey
      = ppr_co FunPrec arg : split res
    split co = [ppr_co TopPrec co]

ppr_forall_co :: Prec -> Coercion -> SDoc
ppr_forall_co p ty
  = maybeParen p FunPrec $
    sep [pprForAll tvs, ppr_co TopPrec rho]
  where
    (tvs,  rho) = split1 [] ty
    split1 tvs (ForAllCo tv ty) = split1 (tv:tvs) ty
    split1 tvs ty               = (reverse tvs, ty)
\end{code}

\begin{code}
pprCoAxiom :: CoAxiom br -> SDoc
pprCoAxiom ax@(CoAxiom { co_ax_tc = tc, co_ax_branches = branches })
  = hang (ptext (sLit "axiom") <+> ppr ax <+> dcolon)
       2 (vcat (map (pprCoAxBranch tc) $ fromBranchList branches))

pprCoAxBranch :: TyCon -> CoAxBranch -> SDoc
pprCoAxBranch fam_tc (CoAxBranch { cab_tvs = tvs
                                 , cab_lhs = lhs
                                 , cab_rhs = rhs })
  = hang (ifPprDebug (pprForAll tvs))
       2 (hang (pprTypeApp fam_tc lhs) 2 (equals <+> (ppr rhs)))

pprCoAxBranchHdr :: CoAxiom br -> BranchIndex -> SDoc
pprCoAxBranchHdr ax@(CoAxiom { co_ax_tc = fam_tc, co_ax_name = name }) index
  | CoAxBranch { cab_lhs = tys, cab_loc = loc } <- coAxiomNthBranch ax index
  = hang (pprTypeApp fam_tc tys)
       2 (ptext (sLit "-- Defined") <+> ppr_loc loc)
  where
        ppr_loc loc
          | isGoodSrcSpan loc
          = ptext (sLit "at") <+> ppr (srcSpanStart loc)
    
          | otherwise
          = ptext (sLit "in") <+>
              quotes (ppr (nameModule name))
\end{code}

%************************************************************************
%*									*
	Functions over Kinds		
%*									*
%************************************************************************

\begin{code}
-- | This breaks a 'Coercion' with type @T A B C ~ T D E F@ into
-- a list of 'Coercion's of kinds @A ~ D@, @B ~ E@ and @E ~ F@. Hence:
--
-- > decomposeCo 3 c = [nth 0 c, nth 1 c, nth 2 c]
decomposeCo :: Arity -> Coercion -> [Coercion]
decomposeCo arity co 
  = [mkNthCo n co | n <- [0..(arity-1)] ]
           -- Remember, Nth is zero-indexed

-- | Attempts to obtain the type variable underlying a 'Coercion'
getCoVar_maybe :: Coercion -> Maybe CoVar
getCoVar_maybe (CoVarCo cv) = Just cv  
getCoVar_maybe _            = Nothing

-- | Attempts to tease a coercion apart into a type constructor and the application
-- of a number of coercion arguments to that constructor
splitTyConAppCo_maybe :: Coercion -> Maybe (TyCon, [Coercion])
splitTyConAppCo_maybe (Refl ty)           = (fmap . second . map) Refl (splitTyConApp_maybe ty)
splitTyConAppCo_maybe (TyConAppCo tc cos) = Just (tc, cos)
splitTyConAppCo_maybe _                   = Nothing

splitAppCo_maybe :: Coercion -> Maybe (Coercion, Coercion)
-- ^ Attempt to take a coercion application apart.
splitAppCo_maybe (AppCo co1 co2) = Just (co1, co2)
splitAppCo_maybe (TyConAppCo tc cos)
  | isDecomposableTyCon tc || cos `lengthExceeds` tyConArity tc 
  , Just (cos', co') <- snocView cos
  = Just (mkTyConAppCo tc cos', co')    -- Never create unsaturated type family apps!
       -- Use mkTyConAppCo to preserve the invariant
       --  that identity coercions are always represented by Refl
splitAppCo_maybe (Refl ty) 
  | Just (ty1, ty2) <- splitAppTy_maybe ty 
  = Just (Refl ty1, Refl ty2)
splitAppCo_maybe _ = Nothing

splitForAllCo_maybe :: Coercion -> Maybe (TyVar, Coercion)
splitForAllCo_maybe (ForAllCo tv co) = Just (tv, co)
splitForAllCo_maybe _                = Nothing

-------------------------------------------------------
-- and some coercion kind stuff

coVarKind :: CoVar -> (Type,Type) 
coVarKind cv
 | Just (tc, [_kind,ty1,ty2]) <- splitTyConApp_maybe (varType cv)
 = ASSERT(tc `hasKey` eqPrimTyConKey)
   (ty1,ty2)
 | otherwise = panic "coVarKind, non coercion variable"

-- | Makes a coercion type from two types: the types whose equality 
-- is proven by the relevant 'Coercion'
mkCoercionType :: Type -> Type -> Type
mkCoercionType = mkPrimEqPred

isReflCo :: Coercion -> Bool
isReflCo (Refl {}) = True
isReflCo _         = False

isReflCo_maybe :: Coercion -> Maybe Type
isReflCo_maybe (Refl ty) = Just ty
isReflCo_maybe _         = Nothing
\end{code}

%************************************************************************
%*									*
            Building coercions
%*									*
%************************************************************************

\begin{code}
mkCoVarCo :: CoVar -> Coercion
-- cv :: s ~# t
mkCoVarCo cv
  | ty1 `eqType` ty2 = Refl ty1
  | otherwise        = CoVarCo cv
  where
    (ty1, ty2) = ASSERT( isCoVar cv ) coVarKind cv

mkReflCo :: Type -> Coercion
mkReflCo = Refl

mkAxInstCo :: CoAxiom br -> BranchIndex -> [Type] -> Coercion
-- mkAxInstCo can legitimately be called over-staturated; 
-- i.e. with more type arguments than the coercion requires
mkAxInstCo ax index tys
  | arity == n_tys = AxiomInstCo ax_br index rtys
  | otherwise      = ASSERT( arity < n_tys )
                     foldl AppCo (AxiomInstCo ax_br index (take arity rtys))
                                 (drop arity rtys)
  where
    n_tys = length tys
    arity = coAxiomArity ax index
    rtys  = map Refl tys
    ax_br = toBranchedAxiom ax

-- to be used only with unbranched axioms
mkUnbranchedAxInstCo :: CoAxiom Unbranched -> [Type] -> Coercion
mkUnbranchedAxInstCo ax tys
  = mkAxInstCo ax 0 tys

mkAxInstLHS, mkAxInstRHS :: CoAxiom br -> BranchIndex -> [Type] -> Type
-- Instantiate the axiom with specified types,
-- returning the instantiated RHS
-- A companion to mkAxInstCo: 
--    mkAxInstRhs ax index tys = snd (coercionKind (mkAxInstCo ax index tys))
mkAxInstLHS ax index tys
  | CoAxBranch { cab_tvs = tvs, cab_lhs = lhs } <- coAxiomNthBranch ax index
  , (tys1, tys2) <- splitAtList tvs tys
  = ASSERT( tvs `equalLength` tys1 ) 
    mkTyConApp (coAxiomTyCon ax) (substTysWith tvs tys1 lhs ++ tys2)

mkAxInstRHS ax index tys
  | CoAxBranch { cab_tvs = tvs, cab_rhs = rhs } <- coAxiomNthBranch ax index
  , (tys1, tys2) <- splitAtList tvs tys
  = ASSERT( tvs `equalLength` tys1 ) 
    mkAppTys (substTyWith tvs tys1 rhs) tys2

mkUnbranchedAxInstRHS :: CoAxiom Unbranched -> [Type] -> Type
mkUnbranchedAxInstRHS ax = mkAxInstRHS ax 0

-- | Apply a 'Coercion' to another 'Coercion'.
mkAppCo :: Coercion -> Coercion -> Coercion
mkAppCo (Refl ty1) (Refl ty2)       = Refl (mkAppTy ty1 ty2)
mkAppCo (Refl (TyConApp tc tys)) co = TyConAppCo tc (map Refl tys ++ [co])
mkAppCo (TyConAppCo tc cos) co      = TyConAppCo tc (cos ++ [co])
mkAppCo co1 co2                     = AppCo co1 co2
-- Note, mkAppCo is careful to maintain invariants regarding
-- where Refl constructors appear; see the comments in the definition
-- of Coercion and the Note [Refl invariant] in types/TypeRep.lhs.

-- | Applies multiple 'Coercion's to another 'Coercion', from left to right.
-- See also 'mkAppCo'
mkAppCos :: Coercion -> [Coercion] -> Coercion
mkAppCos co1 tys = foldl mkAppCo co1 tys

-- | Apply a type constructor to a list of coercions.
mkTyConAppCo :: TyCon -> [Coercion] -> Coercion
mkTyConAppCo tc cos
	       -- Expand type synonyms
  | Just (tv_co_prs, rhs_ty, leftover_cos) <- tcExpandTyCon_maybe tc cos
  = mkAppCos (liftCoSubst tv_co_prs rhs_ty) leftover_cos

  | Just tys <- traverse isReflCo_maybe cos 
  = Refl (mkTyConApp tc tys)	-- See Note [Refl invariant]

  | otherwise = TyConAppCo tc cos

-- | Make a function 'Coercion' between two other 'Coercion's
mkFunCo :: Coercion -> Coercion -> Coercion
mkFunCo co1 co2 = mkTyConAppCo funTyCon [co1, co2]

-- | Make a 'Coercion' which binds a variable within an inner 'Coercion'
mkForAllCo :: Var -> Coercion -> Coercion
-- note that a TyVar should be used here, not a CoVar (nor a TcTyVar)
mkForAllCo tv (Refl ty) = ASSERT( isTyVar tv ) Refl (mkForAllTy tv ty)
mkForAllCo tv  co       = ASSERT( isTyVar tv ) ForAllCo tv co

-------------------------------

-- | Create a symmetric version of the given 'Coercion' that asserts
--   equality between the same types but in the other "direction", so
--   a kind of @t1 ~ t2@ becomes the kind @t2 ~ t1@.
mkSymCo :: Coercion -> Coercion

-- Do a few simple optimizations, but don't bother pushing occurrences
-- of symmetry to the leaves; the optimizer will take care of that.
mkSymCo co@(Refl {})              = co
mkSymCo    (UnsafeCo ty1 ty2)    = UnsafeCo ty2 ty1
mkSymCo    (SymCo co)            = co
mkSymCo co                       = SymCo co

-- | Create a new 'Coercion' by composing the two given 'Coercion's transitively.
mkTransCo :: Coercion -> Coercion -> Coercion
mkTransCo (Refl _) co = co
mkTransCo co (Refl _) = co
mkTransCo co1 co2     = TransCo co1 co2

mkNthCo :: Int -> Coercion -> Coercion
mkNthCo n (Refl ty) = ASSERT( ok_tc_app ty n ) 
                      Refl (tyConAppArgN n ty)
mkNthCo n co        = ASSERT( ok_tc_app _ty1 n && ok_tc_app _ty2 n )
                      NthCo n co
                    where
                      Pair _ty1 _ty2 = coercionKind co

mkLRCo :: LeftOrRight -> Coercion -> Coercion
mkLRCo lr (Refl ty) = Refl (pickLR lr (splitAppTy ty))
mkLRCo lr co        = LRCo lr co

ok_tc_app :: Type -> Int -> Bool
ok_tc_app ty n = case splitTyConApp_maybe ty of
                   Just (_, tys) -> tys `lengthExceeds` n
                   Nothing       -> False

-- | Instantiates a 'Coercion' with a 'Type' argument. 
mkInstCo :: Coercion -> Type -> Coercion
mkInstCo co ty = InstCo co ty

-- | Manufacture a coercion from thin air. Needless to say, this is
--   not usually safe, but it is used when we know we are dealing with
--   bottom, which is one case in which it is safe.  This is also used
--   to implement the @unsafeCoerce#@ primitive.  Optimise by pushing
--   down through type constructors.
mkUnsafeCo :: Type -> Type -> Coercion
mkUnsafeCo ty1 ty2 | ty1 `eqType` ty2 = Refl ty1
mkUnsafeCo (TyConApp tc1 tys1) (TyConApp tc2 tys2)
  | tc1 == tc2
  = mkTyConAppCo tc1 (zipWith mkUnsafeCo tys1 tys2)

mkUnsafeCo (FunTy a1 r1) (FunTy a2 r2)
  = mkFunCo (mkUnsafeCo a1 a2) (mkUnsafeCo r1 r2)

mkUnsafeCo ty1 ty2 = UnsafeCo ty1 ty2

-- See note [Newtype coercions] in TyCon

-- | Create a coercion constructor (axiom) suitable for the given
--   newtype 'TyCon'. The 'Name' should be that of a new coercion
--   'CoAxiom', the 'TyVar's the arguments expected by the @newtype@ and
--   the type the appropriate right hand side of the @newtype@, with
--   the free variables a subset of those 'TyVar's.
mkNewTypeCo :: Name -> TyCon -> [TyVar] -> Type -> CoAxiom Unbranched
mkNewTypeCo name tycon tvs rhs_ty
  = CoAxiom { co_ax_unique   = nameUnique name
            , co_ax_name     = name
            , co_ax_implicit = True  -- See Note [Implicit axioms] in TyCon
            , co_ax_tc       = tycon
            , co_ax_branches = FirstBranch branch }
  where branch = CoAxBranch { cab_loc = getSrcSpan name
                            , cab_tvs = tvs
                            , cab_lhs = mkTyVarTys tvs
                            , cab_rhs = rhs_ty
                            , cab_incomps = [] }

mkPiCos :: [Var] -> Coercion -> Coercion
mkPiCos vs co = foldr mkPiCo co vs

mkPiCo  :: Var -> Coercion -> Coercion
mkPiCo v co | isTyVar v = mkForAllCo v co
            | otherwise = mkFunCo (mkReflCo (varType v)) co

mkCoCast :: Coercion -> Coercion -> Coercion
-- (mkCoCast (c :: s1 ~# t1) (g :: (s1 ~# t1) ~# (s2 ~# t2)
mkCoCast c g
  = mkSymCo g1 `mkTransCo` c `mkTransCo` g2
  where
       -- g  :: (s1 ~# s2) ~# (t1 ~#  t2)
       -- g1 :: s1 ~# t1
       -- g2 :: s2 ~# t2
    [_reflk, g1, g2] = decomposeCo 3 g
            -- Remember, (~#) :: forall k. k -> k -> *
            -- so it takes *three* arguments, not two
\end{code}

%************************************************************************
%*									*
            Newtypes
%*									*
%************************************************************************

\begin{code}
instNewTyCon_maybe :: TyCon -> [Type] -> Maybe (Type, Coercion)
-- ^ If @co :: T ts ~ rep_ty@ then:
--
-- > instNewTyCon_maybe T ts = Just (rep_ty, co)
-- Checks for a newtype, and for being saturated
instNewTyCon_maybe tc tys
  | Just (tvs, ty, co_tc) <- unwrapNewTyCon_maybe tc  -- Check for newtype
  , tys `lengthIs` tyConArity tc                      -- Check saturated
  = Just (substTyWith tvs tys ty, mkUnbranchedAxInstCo co_tc tys)
  | otherwise
  = Nothing

splitNewTypeRepCo_maybe :: Type -> Maybe (Type, Coercion)  
-- ^ Sometimes we want to look through a @newtype@ and get its associated coercion.
-- This function only strips *one layer* of @newtype@ off, so the caller will usually call
-- itself recursively. If
--
-- > splitNewTypeRepCo_maybe ty = Just (ty', co)
--
-- then  @co : ty ~ ty'@.  The function returns @Nothing@ for non-@newtypes@, 
-- or unsaturated applications
splitNewTypeRepCo_maybe ty 
  | Just ty' <- coreView ty 
  = splitNewTypeRepCo_maybe ty'
splitNewTypeRepCo_maybe (TyConApp tc tys)
  = instNewTyCon_maybe tc tys
splitNewTypeRepCo_maybe _
  = Nothing

topNormaliseNewType :: Type -> Maybe (Type, Coercion)
topNormaliseNewType ty
  = case topNormaliseNewTypeX initRecTc ty of
      Just (_, co, ty) -> Just (ty, co)
      Nothing          -> Nothing

topNormaliseNewTypeX :: RecTcChecker -> Type -> Maybe (RecTcChecker, Coercion, Type)
topNormaliseNewTypeX rec_nts ty
  | Just ty' <- coreView ty         -- Expand predicates and synonyms
  = topNormaliseNewTypeX rec_nts ty'

topNormaliseNewTypeX rec_nts (TyConApp tc tys)
  | Just rec_nts'     <- checkRecTc rec_nts tc  -- See Note [Expanding newtypes] in TyCon
  , Just (rep_ty, co) <- instNewTyCon_maybe tc tys
  = case topNormaliseNewTypeX rec_nts' rep_ty of
       Nothing                       -> Just (rec_nts', co,                 rep_ty)
       Just (rec_nts', co', rep_ty') -> Just (rec_nts', co `mkTransCo` co', rep_ty')

topNormaliseNewTypeX _ _ = Nothing
\end{code}


%************************************************************************
%*									*
                   Equality of coercions
%*                                                                      *
%************************************************************************

\begin{code}
-- | Determines syntactic equality of coercions
coreEqCoercion :: Coercion -> Coercion -> Bool
coreEqCoercion co1 co2 = coreEqCoercion2 rn_env co1 co2
  where rn_env = mkRnEnv2 (mkInScopeSet (tyCoVarsOfCo co1 `unionVarSet` tyCoVarsOfCo co2))

coreEqCoercion2 :: RnEnv2 -> Coercion -> Coercion -> Bool
coreEqCoercion2 env (Refl ty1) (Refl ty2) = eqTypeX env ty1 ty2
coreEqCoercion2 env (TyConAppCo tc1 cos1) (TyConAppCo tc2 cos2)
  = tc1 == tc2 && all2 (coreEqCoercion2 env) cos1 cos2

coreEqCoercion2 env (AppCo co11 co12) (AppCo co21 co22)
  = coreEqCoercion2 env co11 co21 && coreEqCoercion2 env co12 co22

coreEqCoercion2 env (ForAllCo v1 co1) (ForAllCo v2 co2)
  = coreEqCoercion2 (rnBndr2 env v1 v2) co1 co2

coreEqCoercion2 env (CoVarCo cv1) (CoVarCo cv2)
  = rnOccL env cv1 == rnOccR env cv2

coreEqCoercion2 env (AxiomInstCo con1 ind1 cos1) (AxiomInstCo con2 ind2 cos2)
  = con1 == con2
    && ind1 == ind2
    && all2 (coreEqCoercion2 env) cos1 cos2

coreEqCoercion2 env (UnsafeCo ty11 ty12) (UnsafeCo ty21 ty22)
  = eqTypeX env ty11 ty21 && eqTypeX env ty12 ty22

coreEqCoercion2 env (SymCo co1) (SymCo co2)
  = coreEqCoercion2 env co1 co2

coreEqCoercion2 env (TransCo co11 co12) (TransCo co21 co22)
  = coreEqCoercion2 env co11 co21 && coreEqCoercion2 env co12 co22

coreEqCoercion2 env (NthCo d1 co1) (NthCo d2 co2)
  = d1 == d2 && coreEqCoercion2 env co1 co2
coreEqCoercion2 env (LRCo d1 co1) (LRCo d2 co2)
  = d1 == d2 && coreEqCoercion2 env co1 co2

coreEqCoercion2 env (InstCo co1 ty1) (InstCo co2 ty2)
  = coreEqCoercion2 env co1 co2 && eqTypeX env ty1 ty2

coreEqCoercion2 _ _ _ = False
\end{code}

%************************************************************************
%*									*
                   Substitution of coercions
%*                                                                      *
%************************************************************************

\begin{code}
-- | A substitution of 'Coercion's for 'CoVar's (OR 'TyVar's, when
--   doing a \"lifting\" substitution)
type CvSubstEnv = VarEnv Coercion

emptyCvSubstEnv :: CvSubstEnv
emptyCvSubstEnv = emptyVarEnv

data CvSubst 		
  = CvSubst InScopeSet 	-- The in-scope type variables
	    TvSubstEnv	-- Substitution of types
            CvSubstEnv  -- Substitution of coercions

instance Outputable CvSubst where
  ppr (CvSubst ins tenv cenv)
    = brackets $ sep[ ptext (sLit "CvSubst"),
		      nest 2 (ptext (sLit "In scope:") <+> ppr ins), 
		      nest 2 (ptext (sLit "Type env:") <+> ppr tenv),
		      nest 2 (ptext (sLit "Coercion env:") <+> ppr cenv) ]

emptyCvSubst :: CvSubst
emptyCvSubst = CvSubst emptyInScopeSet emptyVarEnv emptyVarEnv

isEmptyCvSubst :: CvSubst -> Bool
isEmptyCvSubst (CvSubst _ tenv cenv) = isEmptyVarEnv tenv && isEmptyVarEnv cenv

getCvInScope :: CvSubst -> InScopeSet
getCvInScope (CvSubst in_scope _ _) = in_scope

zapCvSubstEnv :: CvSubst -> CvSubst
zapCvSubstEnv (CvSubst in_scope _ _) = CvSubst in_scope emptyVarEnv emptyVarEnv

cvTvSubst :: CvSubst -> TvSubst
cvTvSubst (CvSubst in_scope tvs _) = TvSubst in_scope tvs

tvCvSubst :: TvSubst -> CvSubst
tvCvSubst (TvSubst in_scope tenv) = CvSubst in_scope tenv emptyCvSubstEnv

extendTvSubst :: CvSubst -> TyVar -> Type -> CvSubst
extendTvSubst (CvSubst in_scope tenv cenv) tv ty
  = CvSubst in_scope (extendVarEnv tenv tv ty) cenv

extendCvSubstAndInScope :: CvSubst -> CoVar -> Coercion -> CvSubst
-- Also extends the in-scope set
extendCvSubstAndInScope (CvSubst in_scope tenv cenv) cv co
  = CvSubst (in_scope `extendInScopeSetSet` tyCoVarsOfCo co)
            tenv
            (extendVarEnv cenv cv co)

substCoVarBndr :: CvSubst -> CoVar -> (CvSubst, CoVar)
substCoVarBndr subst@(CvSubst in_scope tenv cenv) old_var
  = ASSERT( isCoVar old_var )
    (CvSubst (in_scope `extendInScopeSet` new_var) tenv new_cenv, new_var)
  where
    -- When we substitute (co :: t1 ~ t2) we may get the identity (co :: t ~ t)
    -- In that case, mkCoVarCo will return a ReflCoercion, and
    -- we want to substitute that (not new_var) for old_var
    new_co    = mkCoVarCo new_var
    no_change = new_var == old_var && not (isReflCo new_co)

    new_cenv | no_change = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var new_co

    new_var = uniqAway in_scope subst_old_var
    subst_old_var = mkCoVar (varName old_var) (substTy subst (varType old_var))
		  -- It's important to do the substitution for coercions,
		  -- because they can have free type variables

substTyVarBndr :: CvSubst -> TyVar -> (CvSubst, TyVar)
substTyVarBndr (CvSubst in_scope tenv cenv) old_var
  = case Type.substTyVarBndr (TvSubst in_scope tenv) old_var of
      (TvSubst in_scope' tenv', new_var) -> (CvSubst in_scope' tenv' cenv, new_var)

mkCvSubst :: InScopeSet -> [(Var,Coercion)] -> CvSubst
mkCvSubst in_scope prs = CvSubst in_scope Type.emptyTvSubstEnv (mkVarEnv prs)

zipOpenCvSubst :: [Var] -> [Coercion] -> CvSubst
zipOpenCvSubst vs cos
  | debugIsOn && (length vs /= length cos)
  = pprTrace "zipOpenCvSubst" (ppr vs $$ ppr cos) emptyCvSubst
  | otherwise 
  = CvSubst (mkInScopeSet (tyCoVarsOfCos cos)) emptyTvSubstEnv (zipVarEnv vs cos)

substCoWithTy :: InScopeSet -> TyVar -> Type -> Coercion -> Coercion
substCoWithTy in_scope tv ty = substCoWithTys in_scope [tv] [ty]

substCoWithTys :: InScopeSet -> [TyVar] -> [Type] -> Coercion -> Coercion
substCoWithTys in_scope tvs tys co
  | debugIsOn && (length tvs /= length tys)
  = pprTrace "substCoWithTys" (ppr tvs $$ ppr tys) co
  | otherwise 
  = ASSERT( length tvs == length tys )
    substCo (CvSubst in_scope (zipVarEnv tvs tys) emptyVarEnv) co

-- | Substitute within a 'Coercion'
substCo :: CvSubst -> Coercion -> Coercion
substCo subst co | isEmptyCvSubst subst = co
                 | otherwise            = subst_co subst co

-- | Substitute within several 'Coercion's
substCos :: CvSubst -> [Coercion] -> [Coercion]
substCos subst cos | isEmptyCvSubst subst = cos
                   | otherwise            = map (substCo subst) cos

substTy :: CvSubst -> Type -> Type
substTy subst = Type.substTy (cvTvSubst subst)

subst_co :: CvSubst -> Coercion -> Coercion
subst_co subst co
  = go co
  where
    go_ty :: Type -> Type
    go_ty = Coercion.substTy subst

    go :: Coercion -> Coercion
    go (Refl ty)             = Refl $! go_ty ty
    go (TyConAppCo tc cos)   = let args = map go cos
                               in  args `seqList` TyConAppCo tc args
    go (AppCo co1 co2)       = mkAppCo (go co1) $! go co2
    go (ForAllCo tv co)      = case substTyVarBndr subst tv of
                                 (subst', tv') ->
                                   ForAllCo tv' $! subst_co subst' co
    go (CoVarCo cv)          = substCoVar subst cv
    go (AxiomInstCo con ind cos) = AxiomInstCo con ind $! map go cos
    go (UnsafeCo ty1 ty2)    = (UnsafeCo $! go_ty ty1) $! go_ty ty2
    go (SymCo co)            = mkSymCo (go co)
    go (TransCo co1 co2)     = mkTransCo (go co1) (go co2)
    go (NthCo d co)          = mkNthCo d (go co)
    go (LRCo lr co)          = mkLRCo lr (go co)
    go (InstCo co ty)        = mkInstCo (go co) $! go_ty ty

substCoVar :: CvSubst -> CoVar -> Coercion
substCoVar (CvSubst in_scope _ cenv) cv
  | Just co  <- lookupVarEnv cenv cv      = co
  | Just cv1 <- lookupInScope in_scope cv = ASSERT( isCoVar cv1 ) CoVarCo cv1
  | otherwise = WARN( True, ptext (sLit "substCoVar not in scope") <+> ppr cv $$ ppr in_scope)
                ASSERT( isCoVar cv ) CoVarCo cv

substCoVars :: CvSubst -> [CoVar] -> [Coercion]
substCoVars subst cvs = map (substCoVar subst) cvs

lookupTyVar :: CvSubst -> TyVar  -> Maybe Type
lookupTyVar (CvSubst _ tenv _) tv = lookupVarEnv tenv tv

lookupCoVar :: CvSubst -> Var  -> Maybe Coercion
lookupCoVar (CvSubst _ _ cenv) v = lookupVarEnv cenv v
\end{code}

%************************************************************************
%*									*
                   "Lifting" substitution
	   [(TyVar,Coercion)] -> Type -> Coercion
%*                                                                      *
%************************************************************************

Note [Lifting coercions over types: liftCoSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The KPUSH rule deals with this situation
   data T a = MkK (a -> Maybe a)
   g :: T t1 ~ K t2
   x :: t1 -> Maybe t1

   case (K @t1 x) |> g of
     K (y:t2 -> Maybe t2) -> rhs

We want to push the coercion inside the constructor application.  
So we do this

   g' :: t1~t2  =  Nth 0 g

   case K @t2 (x |> g' -> Maybe g') of
     K (y:t2 -> Maybe t2) -> rhs

The crucial operation is that we 
  * take the type of K's argument: a -> Maybe a
  * and substitute g' for a
thus giving *coercion*.  This is what liftCoSubst does.

Note [Substituting kinds in liftCoSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to take care with kind polymorphism.  Suppose
  K :: forall k (a:k). (forall b:k. a -> b) -> T k a

Now given  (K @kk1 @ty1 v) |> g) where
  g :: T kk1 ty1 ~ T kk2 ty2
we want to compute
   (forall b:k a->b) [ Nth 0 g/k, Nth 1 g/a ]
Notice that we MUST substitute for 'k'; this happens in
liftCoSubstTyVarBndr.  But what should we substitute?
We need to take b's kind 'k' and return a Kind, not a Coercion!

Happily we can do this because we know that all kind coercions
((Nth 0 g) in this case) are Refl.  So we need a special purpose
   subst_kind: LiftCoSubst -> Kind -> Kind
that expects a Refl coercion (or something equivalent to Refl)
when it looks up a kind variable.

\begin{code}
-- ----------------------------------------------------
-- See Note [Lifting coercions over types: liftCoSubst]
-- ----------------------------------------------------

data LiftCoSubst = LCS InScopeSet LiftCoEnv

type LiftCoEnv = VarEnv Coercion
     -- Maps *type variables* to *coercions*
     -- That's the whole point of this function!

liftCoSubstWith :: [TyVar] -> [Coercion] -> Type -> Coercion
liftCoSubstWith tvs cos ty
  = liftCoSubst (zipEqual "liftCoSubstWith" tvs cos) ty

liftCoSubst :: [(TyVar,Coercion)] -> Type -> Coercion
liftCoSubst prs ty
 | null prs  = Refl ty
 | otherwise = ty_co_subst (LCS (mkInScopeSet (tyCoVarsOfCos (map snd prs)))
                                (mkVarEnv prs)) ty

-- | The \"lifting\" operation which substitutes coercions for type
--   variables in a type to produce a coercion.
--
--   For the inverse operation, see 'liftCoMatch' 
ty_co_subst :: LiftCoSubst -> Type -> Coercion
ty_co_subst subst ty
  = go ty
  where
    go (TyVarTy tv)      = liftCoSubstTyVar subst tv `orElse` Refl (TyVarTy tv)
       			     -- A type variable from a non-cloned forall
			     -- won't be in the substitution
    go (AppTy ty1 ty2)   = mkAppCo (go ty1) (go ty2)
    go (TyConApp tc tys) = mkTyConAppCo tc (map go tys)
                           -- IA0_NOTE: Do we need to do anything
                           -- about kind instantiations? I don't think
                           -- so.  see Note [Kind coercions]
    go (FunTy ty1 ty2)   = mkFunCo (go ty1) (go ty2)
    go (ForAllTy v ty)   = mkForAllCo v' $! (ty_co_subst subst' ty)
                         where
                           (subst', v') = liftCoSubstTyVarBndr subst v
    go ty@(LitTy {})     = mkReflCo ty

liftCoSubstTyVar :: LiftCoSubst -> TyVar -> Maybe Coercion
liftCoSubstTyVar (LCS _ cenv) tv = lookupVarEnv cenv tv 

liftCoSubstTyVarBndr :: LiftCoSubst -> TyVar -> (LiftCoSubst, TyVar)
liftCoSubstTyVarBndr subst@(LCS in_scope cenv) old_var
  = (LCS (in_scope `extendInScopeSet` new_var) new_cenv, new_var)		
  where
    new_cenv | no_change = delVarEnv cenv old_var
	     | otherwise = extendVarEnv cenv old_var (Refl (TyVarTy new_var))

    no_change = no_kind_change && (new_var == old_var)

    new_var1 = uniqAway in_scope old_var

    old_ki = tyVarKind old_var
    no_kind_change = isEmptyVarSet (tyVarsOfType old_ki)
    new_var | no_kind_change = new_var1
            | otherwise      = setTyVarKind new_var1 (subst_kind subst old_ki)

subst_kind :: LiftCoSubst -> Kind -> Kind
-- See Note [Substituting kinds in liftCoSubst]
subst_kind subst@(LCS _ cenv) kind
  = go kind
  where
    go (LitTy n)         = n `seq` LitTy n
    go (TyVarTy kv)      = subst_kv kv
    go (TyConApp tc tys) = let args = map go tys
                           in  args `seqList` TyConApp tc args

    go (FunTy arg res)   = (FunTy $! (go arg)) $! (go res)
    go (AppTy fun arg)   = mkAppTy (go fun) $! (go arg)
    go (ForAllTy tv ty)  = case liftCoSubstTyVarBndr subst tv of
                              (subst', tv') ->
                                 ForAllTy tv' $! (subst_kind subst' ty)

    subst_kv kv
      | Just co <- lookupVarEnv cenv kv
      , let co_kind = coercionKind co
      = ASSERT2( pFst co_kind `eqKind` pSnd co_kind, ppr kv $$ ppr co )
        pFst co_kind
      | otherwise
      = TyVarTy kv
\end{code}

\begin{code}
-- | 'liftCoMatch' is sort of inverse to 'liftCoSubst'.  In particular, if
--   @liftCoMatch vars ty co == Just s@, then @tyCoSubst s ty == co@.
--   That is, it matches a type against a coercion of the same
--   "shape", and returns a lifting substitution which could have been
--   used to produce the given coercion from the given type.
liftCoMatch :: TyVarSet -> Type -> Coercion -> Maybe LiftCoSubst
liftCoMatch tmpls ty co 
  = case ty_co_match menv emptyVarEnv ty co of
      Just cenv -> Just (LCS in_scope cenv)
      Nothing   -> Nothing
  where
    menv     = ME { me_tmpls = tmpls, me_env = mkRnEnv2 in_scope }
    in_scope = mkInScopeSet (tmpls `unionVarSet` tyCoVarsOfCo co)
    -- Like tcMatchTy, assume all the interesting variables 
    -- in ty are in tmpls

-- | 'ty_co_match' does all the actual work for 'liftCoMatch'.
ty_co_match :: MatchEnv -> LiftCoEnv -> Type -> Coercion -> Maybe LiftCoEnv
ty_co_match menv subst ty co 
  | Just ty' <- coreView ty = ty_co_match menv subst ty' co

  -- Match a type variable against a non-refl coercion
ty_co_match menv cenv (TyVarTy tv1) co
  | Just co1' <- lookupVarEnv cenv tv1'      -- tv1' is already bound to co1
  = if coreEqCoercion2 (nukeRnEnvL rn_env) co1' co
    then Just cenv
    else Nothing       -- no match since tv1 matches two different coercions

  | tv1' `elemVarSet` me_tmpls menv           -- tv1' is a template var
  = if any (inRnEnvR rn_env) (varSetElems (tyCoVarsOfCo co))
    then Nothing      -- occurs check failed
    else return (extendVarEnv cenv tv1' co)
        -- BAY: I don't think we need to do any kind matching here yet
        -- (compare 'match'), but we probably will when moving to SHE.

  | otherwise    -- tv1 is not a template ty var, so the only thing it
                 -- can match is a reflexivity coercion for itself.
		 -- But that case is dealt with already
  = Nothing

  where
    rn_env = me_env menv
    tv1' = rnOccL rn_env tv1

ty_co_match menv subst (AppTy ty1 ty2) co
  | Just (co1, co2) <- splitAppCo_maybe co	-- c.f. Unify.match on AppTy
  = do { subst' <- ty_co_match menv subst ty1 co1 
       ; ty_co_match menv subst' ty2 co2 }

ty_co_match menv subst (TyConApp tc1 tys) (TyConAppCo tc2 cos)
  | tc1 == tc2 = ty_co_matches menv subst tys cos

ty_co_match menv subst (FunTy ty1 ty2) (TyConAppCo tc cos)
  | tc == funTyCon = ty_co_matches menv subst [ty1,ty2] cos

ty_co_match menv subst (ForAllTy tv1 ty) (ForAllCo tv2 co) 
  = ty_co_match menv' subst ty co
  where
    menv' = menv { me_env = rnBndr2 (me_env menv) tv1 tv2 }

ty_co_match menv subst ty co
  | Just co' <- pushRefl co = ty_co_match menv subst ty co'
  | otherwise               = Nothing

ty_co_matches :: MatchEnv -> LiftCoEnv -> [Type] -> [Coercion] -> Maybe LiftCoEnv
ty_co_matches menv = matchList (ty_co_match menv)

pushRefl :: Coercion -> Maybe Coercion
pushRefl (Refl (AppTy ty1 ty2))   = Just (AppCo (Refl ty1) (Refl ty2))
pushRefl (Refl (FunTy ty1 ty2))   = Just (TyConAppCo funTyCon [Refl ty1, Refl ty2])
pushRefl (Refl (TyConApp tc tys)) = Just (TyConAppCo tc (map Refl tys))
pushRefl (Refl (ForAllTy tv ty))  = Just (ForAllCo tv (Refl ty))
pushRefl _                        = Nothing
\end{code}

%************************************************************************
%*									*
            Sequencing on coercions
%*									*
%************************************************************************

\begin{code}
seqCo :: Coercion -> ()
seqCo (Refl ty)             = seqType ty
seqCo (TyConAppCo tc cos)   = tc `seq` seqCos cos
seqCo (AppCo co1 co2)       = seqCo co1 `seq` seqCo co2
seqCo (ForAllCo tv co)      = tv `seq` seqCo co
seqCo (CoVarCo cv)          = cv `seq` ()
seqCo (AxiomInstCo con ind cos) = con `seq` ind `seq` seqCos cos
seqCo (UnsafeCo ty1 ty2)    = seqType ty1 `seq` seqType ty2
seqCo (SymCo co)            = seqCo co
seqCo (TransCo co1 co2)     = seqCo co1 `seq` seqCo co2
seqCo (NthCo _ co)          = seqCo co
seqCo (LRCo _ co)           = seqCo co
seqCo (InstCo co ty)        = seqCo co `seq` seqType ty

seqCos :: [Coercion] -> ()
seqCos []       = ()
seqCos (co:cos) = seqCo co `seq` seqCos cos
\end{code}


%************************************************************************
%*									*
	     The kind of a type, and of a coercion
%*									*
%************************************************************************

\begin{code}
coercionType :: Coercion -> Type
coercionType co = case coercionKind co of
                    Pair ty1 ty2 -> mkCoercionType ty1 ty2

------------------
-- | If it is the case that
--
-- > c :: (t1 ~ t2)
--
-- i.e. the kind of @c@ relates @t1@ and @t2@, then @coercionKind c = Pair t1 t2@.

coercionKind :: Coercion -> Pair Type 
coercionKind co = go co
  where 
    go (Refl ty)            = Pair ty ty
    go (TyConAppCo tc cos)  = mkTyConApp tc <$> (sequenceA $ map go cos)
    go (AppCo co1 co2)      = mkAppTy <$> go co1 <*> go co2
    go (ForAllCo tv co)     = mkForAllTy tv <$> go co
    go (CoVarCo cv)         = toPair $ coVarKind cv
    go (AxiomInstCo ax ind cos)
      | CoAxBranch { cab_tvs = tvs, cab_lhs = lhs, cab_rhs = rhs } <- coAxiomNthBranch ax ind
      , Pair tys1 tys2 <- sequenceA (map go cos)
      = ASSERT( cos `equalLength` tvs )  -- Invariant of AxiomInstCo: cos should 
                                         -- exactly saturate the axiom branch
        Pair (substTyWith tvs tys1 (mkTyConApp (coAxiomTyCon ax) lhs))
             (substTyWith tvs tys2 rhs)
    go (UnsafeCo ty1 ty2)   = Pair ty1 ty2
    go (SymCo co)           = swap $ go co
    go (TransCo co1 co2)    = Pair (pFst $ go co1) (pSnd $ go co2)
    go (NthCo d co)         = tyConAppArgN d <$> go co
    go (LRCo lr co)         = (pickLR lr . splitAppTy) <$> go co
    go (InstCo aco ty)      = go_app aco [ty]

    go_app :: Coercion -> [Type] -> Pair Type
    -- Collect up all the arguments and apply all at once
    -- See Note [Nested InstCos]
    go_app (InstCo co ty) tys = go_app co (ty:tys)
    go_app co             tys = (`applyTys` tys) <$> go co

-- | Apply 'coercionKind' to multiple 'Coercion's
coercionKinds :: [Coercion] -> Pair [Type]
coercionKinds tys = sequenceA $ map coercionKind tys
\end{code}

Note [Nested InstCos]
~~~~~~~~~~~~~~~~~~~~~
In Trac #5631 we found that 70% of the entire compilation time was
being spent in coercionKind!  The reason was that we had
   (g @ ty1 @ ty2 .. @ ty100)    -- The "@s" are InstCos
where 
   g :: forall a1 a2 .. a100. phi
If we deal with the InstCos one at a time, we'll do this:
   1.  Find the kind of (g @ ty1 .. @ ty99) : forall a100. phi'
   2.  Substitute phi'[ ty100/a100 ], a single tyvar->type subst
But this is a *quadratic* algorithm, and the blew up Trac #5631.
So it's very important to do the substitution simultaneously.

cf Type.applyTys (which in fact we call here)


\begin{code}
applyCo :: Type -> Coercion -> Type
-- Gives the type of (e co) where e :: (a~b) => ty
applyCo ty co | Just ty' <- coreView ty = applyCo ty' co
applyCo (FunTy _ ty) _ = ty
applyCo _            _ = panic "applyCo"
\end{code}

Note [Kind coercions]
~~~~~~~~~~~~~~~~~~~~~
Kind coercions are only of the form: Refl kind. They are only used to
instantiate kind polymorphic type constructors in TyConAppCo. Remember
that kind instantiation only happens with TyConApp, not AppTy.

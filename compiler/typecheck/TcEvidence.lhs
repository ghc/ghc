%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# LANGUAGE CPP, DeriveDataTypeable #-}

module TcEvidence (

  -- HsWrapper
  HsWrapper(..), 
  (<.>), mkWpTyApps, mkWpEvApps, mkWpEvVarApps, mkWpTyLams, mkWpLams, mkWpLet, mkWpCast,
  idHsWrapper, isIdHsWrapper, pprHsWrapper,

  -- Evidence bindings
  TcEvBinds(..), EvBindsVar(..), 
  EvBindMap(..), emptyEvBindMap, extendEvBinds, lookupEvBind, evBindMapBinds,
  EvBind(..), emptyTcEvBinds, isEmptyTcEvBinds, 
  EvTerm(..), mkEvCast, evVarsOfTerm, 
  EvLit(..), evTermCoercion,

  -- TcCoercion
  TcCoercion(..), LeftOrRight(..), pickLR,
  mkTcReflCo, mkTcNomReflCo, 
  mkTcTyConAppCo, mkTcAppCo, mkTcAppCos, mkTcFunCo,
  mkTcAxInstCo, mkTcUnbranchedAxInstCo, mkTcForAllCo, mkTcForAllCos, 
  mkTcSymCo, mkTcTransCo, mkTcNthCo, mkTcLRCo, mkTcSubCo,
  mkTcAxiomRuleCo,
  tcCoercionKind, coVarsOfTcCo, isEqVar, mkTcCoVarCo, 
  isTcReflCo, getTcCoVar_maybe,
  tcCoercionRole, eqVarRole,
  coercionToTcCoercion
  ) where
#include "HsVersions.h"

import Var
import Coercion( LeftOrRight(..), pickLR, nthRole )
import qualified Coercion as C
import PprCore ()   -- Instance OutputableBndr TyVar
import TypeRep  -- Knows type representation
import TcType
import Type( tyConAppArgN, tyConAppTyCon_maybe, getEqPredTys, getEqPredRole, coAxNthLHS )
import TysPrim( funTyCon )
import TyCon
import CoAxiom
import PrelNames
import VarEnv
import VarSet
import Name

import Util
import Bag
import Pair
import Control.Applicative
import Data.Traversable (traverse, sequenceA)
import qualified Data.Data as Data 
import Outputable
import FastString
import Data.IORef( IORef )
\end{code}


Note [TcCoercions]
~~~~~~~~~~~~~~~~~~
| TcCoercions are a hack used by the typechecker. Normally,
Coercions have free variables of type (a ~# b): we call these
CoVars. However, the type checker passes around equality evidence
(boxed up) at type (a ~ b).

An TcCoercion is simply a Coercion whose free variables have the
boxed type (a ~ b). After we are done with typechecking the
desugarer finds the free variables, unboxes them, and creates a
resulting real Coercion with kosher free variables.

We can use most of the Coercion "smart constructors" to build TcCoercions.
However, mkCoVarCo will not work! The equivalent is mkTcCoVarCo.

The data type is similar to Coercion.Coercion, with the following
differences
  * Most important, TcLetCo adds let-bindings for coercions.
    This is what lets us unify two for-all types and generate
    equality constraints underneath

  * The kind of a TcCoercion is  t1 ~  t2  (resp. Coercible t1 t2)
             of a Coercion   is  t1 ~# t2  (resp. t1 ~#R t2)

  * UnsafeCo aren't required, but we do have TcPhantomCo

  * Representation invariants are weaker:
     - we are allowed to have type synonyms in TcTyConAppCo
     - the first arg of a TcAppCo can be a TcTyConAppCo
     - TcSubCo is not applied as deep as done with mkSubCo
    Reason: they'll get established when we desugar to Coercion

  * TcAxiomInstCo has a [TcCoercion] parameter, and not a [Type] parameter.
    This differs from the formalism, but corresponds to AxiomInstCo (see
    [Coercion axioms applied to coercions]).
    Why can't we use [TcType] here, in code not relevant for the simplifier?
    Because of coercionToTcCoercion.

\begin{code}
data TcCoercion 
  = TcRefl Role TcType
  | TcTyConAppCo Role TyCon [TcCoercion]
  | TcAppCo TcCoercion TcCoercion
  | TcForAllCo TyVar TcCoercion 
  | TcCoVarCo EqVar
  | TcAxiomInstCo (CoAxiom Branched) Int [TcCoercion] -- Int specifies branch number
                                                      -- See [CoAxiom Index] in Coercion.lhs
  -- This is number of types and coercions are expected to match to CoAxiomRule
  -- (i.e., the CoAxiomRules are always fully saturated)
  | TcAxiomRuleCo CoAxiomRule [TcType] [TcCoercion]
  | TcPhantomCo TcType TcType
  | TcSymCo TcCoercion
  | TcTransCo TcCoercion TcCoercion
  | TcNthCo Int TcCoercion
  | TcLRCo LeftOrRight TcCoercion
  | TcSubCo TcCoercion
  | TcCastCo TcCoercion TcCoercion     -- co1 |> co2
  | TcLetCo TcEvBinds TcCoercion
  deriving (Data.Data, Data.Typeable)

isEqVar :: Var -> Bool 
-- Is lifted coercion variable (only!)
isEqVar v = case tyConAppTyCon_maybe (varType v) of
               Just tc -> tc `hasKey` eqTyConKey
               Nothing -> False

isTcReflCo_maybe :: TcCoercion -> Maybe TcType
isTcReflCo_maybe (TcRefl _ ty) = Just ty
isTcReflCo_maybe _             = Nothing

isTcReflCo :: TcCoercion -> Bool
isTcReflCo (TcRefl {}) = True
isTcReflCo _           = False

getTcCoVar_maybe :: TcCoercion -> Maybe CoVar
getTcCoVar_maybe (TcCoVarCo v) = Just v
getTcCoVar_maybe _             = Nothing

mkTcReflCo :: Role -> TcType -> TcCoercion
mkTcReflCo = TcRefl

mkTcNomReflCo :: TcType -> TcCoercion
mkTcNomReflCo = TcRefl Nominal

mkTcFunCo :: Role -> TcCoercion -> TcCoercion -> TcCoercion
mkTcFunCo role co1 co2 = mkTcTyConAppCo role funTyCon [co1, co2]

mkTcTyConAppCo :: Role -> TyCon -> [TcCoercion] -> TcCoercion
mkTcTyConAppCo role tc cos -- No need to expand type synonyms
                           -- See Note [TcCoercions]
  | Just tys <- traverse isTcReflCo_maybe cos 
  = TcRefl role (mkTyConApp tc tys)  -- See Note [Refl invariant]

  | otherwise = TcTyConAppCo role tc cos

-- input coercion is Nominal
-- mkSubCo will do some normalisation. We do not do it for TcCoercions, but
-- defer that to desugaring; just to reduce the code duplication a little bit
mkTcSubCo :: TcCoercion -> TcCoercion
mkTcSubCo co = ASSERT2( tcCoercionRole co == Nominal, ppr co)
               TcSubCo co

maybeTcSubCo2_maybe :: Role   -- desired role
                    -> Role   -- current role
                    -> TcCoercion -> Maybe TcCoercion
maybeTcSubCo2_maybe Representational Nominal = Just . mkTcSubCo
maybeTcSubCo2_maybe Nominal Representational = const Nothing
maybeTcSubCo2_maybe Phantom _                = panic "maybeTcSubCo2_maybe Phantom" -- not supported (not needed at the moment)
maybeTcSubCo2_maybe _ Phantom                = const Nothing
maybeTcSubCo2_maybe _ _                      = Just

maybeTcSubCo2 :: Role  -- desired role
              -> Role  -- current role
              -> TcCoercion -> TcCoercion
maybeTcSubCo2 r1 r2 co
  = case maybeTcSubCo2_maybe r1 r2 co of
      Just co' -> co'
      Nothing  -> pprPanic "maybeTcSubCo2" (ppr r1 <+> ppr r2 <+> ppr co)

mkTcAxInstCo :: Role -> CoAxiom br -> Int -> [TcType] -> TcCoercion
mkTcAxInstCo role ax index tys
  | ASSERT2( not (role == Nominal && ax_role == Representational) , ppr (ax, tys) )
    arity == n_tys = maybeTcSubCo2 role ax_role $ TcAxiomInstCo ax_br index rtys
  | otherwise      = ASSERT( arity < n_tys )
                     maybeTcSubCo2 role ax_role $ 
                     foldl TcAppCo (TcAxiomInstCo ax_br index (take arity rtys))
                                   (drop arity rtys)
  where
    n_tys     = length tys
    ax_br     = toBranchedAxiom ax
    branch    = coAxiomNthBranch ax_br index
    arity     = length $ coAxBranchTyVars branch
    ax_role   = coAxiomRole ax
    arg_roles = coAxBranchRoles branch
    rtys      = zipWith mkTcReflCo (arg_roles ++ repeat Nominal) tys

mkTcAxiomRuleCo :: CoAxiomRule -> [TcType] -> [TcCoercion] -> TcCoercion
mkTcAxiomRuleCo = TcAxiomRuleCo

mkTcUnbranchedAxInstCo :: Role -> CoAxiom Unbranched -> [TcType] -> TcCoercion
mkTcUnbranchedAxInstCo role ax tys
  = mkTcAxInstCo role ax 0 tys

mkTcAppCo :: TcCoercion -> TcCoercion -> TcCoercion
-- No need to deal with TyConApp on the left; see Note [TcCoercions]
mkTcAppCo (TcRefl r ty1) (TcRefl _ ty2) = TcRefl r (mkAppTy ty1 ty2)
mkTcAppCo co1 co2                       = TcAppCo co1 co2

mkTcSymCo :: TcCoercion -> TcCoercion
mkTcSymCo co@(TcRefl {})  = co
mkTcSymCo    (TcSymCo co) = co
mkTcSymCo co              = TcSymCo co

mkTcTransCo :: TcCoercion -> TcCoercion -> TcCoercion
mkTcTransCo (TcRefl {}) co = co
mkTcTransCo co (TcRefl {}) = co
mkTcTransCo co1 co2        = TcTransCo co1 co2

mkTcNthCo :: Int -> TcCoercion -> TcCoercion
mkTcNthCo n (TcRefl r ty) = TcRefl r (tyConAppArgN n ty)
mkTcNthCo n co            = TcNthCo n co

mkTcLRCo :: LeftOrRight -> TcCoercion -> TcCoercion
mkTcLRCo lr (TcRefl r ty) = TcRefl r (pickLR lr (tcSplitAppTy ty))
mkTcLRCo lr co            = TcLRCo lr co

mkTcAppCos :: TcCoercion -> [TcCoercion] -> TcCoercion
mkTcAppCos co1 tys = foldl mkTcAppCo co1 tys

mkTcForAllCo :: Var -> TcCoercion -> TcCoercion
-- note that a TyVar should be used here, not a CoVar (nor a TcTyVar)
mkTcForAllCo tv (TcRefl r ty) = ASSERT( isTyVar tv ) TcRefl r (mkForAllTy tv ty)
mkTcForAllCo tv  co           = ASSERT( isTyVar tv ) TcForAllCo tv co

mkTcForAllCos :: [Var] -> TcCoercion -> TcCoercion
mkTcForAllCos tvs (TcRefl r ty) = ASSERT( all isTyVar tvs ) TcRefl r (mkForAllTys tvs ty)
mkTcForAllCos tvs co            = ASSERT( all isTyVar tvs ) foldr TcForAllCo co tvs

mkTcCoVarCo :: EqVar -> TcCoercion
-- ipv :: s ~ t  (the boxed equality type) or Coercible s t (the boxed representational equality type)
mkTcCoVarCo ipv = TcCoVarCo ipv
  -- Previously I checked for (ty ~ ty) and generated Refl,
  -- but in fact ipv may not even (visibly) have a (t1 ~ t2) type, because
  -- the constraint solver does not substitute in the types of
  -- evidence variables as it goes.  In any case, the optimisation
  -- will be done in the later zonking phase
\end{code}

\begin{code}
tcCoercionKind :: TcCoercion -> Pair Type
tcCoercionKind co = go co 
  where 
    go (TcRefl _ ty)          = Pair ty ty
    go (TcLetCo _ co)         = go co
    go (TcCastCo _ co)        = case getEqPredTys (pSnd (go co)) of
                                   (ty1,ty2) -> Pair ty1 ty2
    go (TcTyConAppCo _ tc cos)= mkTyConApp tc <$> (sequenceA $ map go cos)
    go (TcAppCo co1 co2)      = mkAppTy <$> go co1 <*> go co2
    go (TcForAllCo tv co)     = mkForAllTy tv <$> go co
    go (TcCoVarCo cv)         = eqVarKind cv
    go (TcAxiomInstCo ax ind cos)
      = let branch = coAxiomNthBranch ax ind
            tvs = coAxBranchTyVars branch
            Pair tys1 tys2 = sequenceA (map go cos)
        in ASSERT( cos `equalLength` tvs )
           Pair (substTyWith tvs tys1 (coAxNthLHS ax ind))
                (substTyWith tvs tys2 (coAxBranchRHS branch))
    go (TcPhantomCo ty1 ty2)  = Pair ty1 ty2
    go (TcSymCo co)           = swap (go co)
    go (TcTransCo co1 co2)    = Pair (pFst (go co1)) (pSnd (go co2))
    go (TcNthCo d co)         = tyConAppArgN d <$> go co
    go (TcLRCo lr co)         = (pickLR lr . tcSplitAppTy) <$> go co
    go (TcSubCo co)           = go co
    go (TcAxiomRuleCo ax ts cs) =
       case coaxrProves ax ts (map tcCoercionKind cs) of
         Just res -> res
         Nothing -> panic "tcCoercionKind: malformed TcAxiomRuleCo"

eqVarRole :: EqVar -> Role
eqVarRole cv = getEqPredRole (varType cv)

eqVarKind :: EqVar -> Pair Type
eqVarKind cv
 | Just (tc, [_kind,ty1,ty2]) <- tcSplitTyConApp_maybe (varType cv)
 = ASSERT(tc `hasKey` eqTyConKey)
   Pair ty1 ty2
 | otherwise = pprPanic "eqVarKind, non coercion variable" (ppr cv <+> dcolon <+> ppr (varType cv))

tcCoercionRole :: TcCoercion -> Role
tcCoercionRole = go
  where
    go (TcRefl r _)           = r
    go (TcTyConAppCo r _ _)   = r
    go (TcAppCo co _)         = go co
    go (TcForAllCo _ co)      = go co
    go (TcCoVarCo cv)         = eqVarRole cv
    go (TcAxiomInstCo ax _ _) = coAxiomRole ax
    go (TcPhantomCo _ _)      = Phantom
    go (TcSymCo co)           = go co
    go (TcTransCo co1 _)      = go co1 -- same as go co2
    go (TcNthCo n co)         = let Pair ty1 _ = tcCoercionKind co
                                    (tc, _) = tcSplitTyConApp ty1
                                in nthRole (go co) tc n
    go (TcLRCo _ _)           = Nominal
    go (TcSubCo _)            = Representational
    go (TcAxiomRuleCo c _ _)  = coaxrRole c
    go (TcCastCo c _)         = go c
    go (TcLetCo _ c)          = go c


coVarsOfTcCo :: TcCoercion -> VarSet
-- Only works on *zonked* coercions, because of TcLetCo
coVarsOfTcCo tc_co
  = go tc_co
  where
    go (TcRefl _ _)              = emptyVarSet
    go (TcTyConAppCo _ _ cos)    = foldr (unionVarSet . go) emptyVarSet cos
    go (TcAppCo co1 co2)         = go co1 `unionVarSet` go co2
    go (TcCastCo co1 co2)        = go co1 `unionVarSet` go co2
    go (TcForAllCo _ co)         = go co
    go (TcCoVarCo v)             = unitVarSet v
    go (TcAxiomInstCo _ _ cos)   = foldr (unionVarSet . go) emptyVarSet cos
    go (TcPhantomCo _ _)         = emptyVarSet
    go (TcSymCo co)              = go co
    go (TcTransCo co1 co2)       = go co1 `unionVarSet` go co2
    go (TcNthCo _ co)            = go co
    go (TcLRCo  _ co)            = go co
    go (TcSubCo co)              = go co
    go (TcLetCo (EvBinds bs) co) = foldrBag (unionVarSet . go_bind) (go co) bs
                                   `minusVarSet` get_bndrs bs
    go (TcLetCo {}) = emptyVarSet    -- Harumph. This does legitimately happen in the call
                                     -- to evVarsOfTerm in the DEBUG check of setEvBind
    go (TcAxiomRuleCo _ _ cos)   = foldr (unionVarSet . go) emptyVarSet cos


    -- We expect only coercion bindings, so use evTermCoercion 
    go_bind :: EvBind -> VarSet
    go_bind (EvBind _ tm) = go (evTermCoercion tm)

    get_bndrs :: Bag EvBind -> VarSet
    get_bndrs = foldrBag (\ (EvBind b _) bs -> extendVarSet bs b) emptyVarSet 
\end{code}

Pretty printing

\begin{code}
instance Outputable TcCoercion where
  ppr = pprTcCo

pprTcCo, pprParendTcCo :: TcCoercion -> SDoc
pprTcCo       co = ppr_co TopPrec   co
pprParendTcCo co = ppr_co TyConPrec co

ppr_co :: TyPrec -> TcCoercion -> SDoc
ppr_co _ (TcRefl r ty) = angleBrackets (ppr ty) <> ppr_role r

ppr_co p co@(TcTyConAppCo _ tc [_,_])
  | tc `hasKey` funTyConKey = ppr_fun_co p co

ppr_co p (TcTyConAppCo r tc cos) = pprTcApp   p ppr_co tc cos <> ppr_role r
ppr_co p (TcLetCo bs co)         = maybeParen p TopPrec $
                                   sep [ptext (sLit "let") <+> braces (ppr bs), ppr co]
ppr_co p (TcAppCo co1 co2)       = maybeParen p TyConPrec $
                                   pprTcCo co1 <+> ppr_co TyConPrec co2
ppr_co p (TcCastCo co1 co2)      = maybeParen p FunPrec $
                                   ppr_co FunPrec co1 <+> ptext (sLit "|>") <+> ppr_co FunPrec co2
ppr_co p co@(TcForAllCo {})      = ppr_forall_co p co
                     
ppr_co _ (TcCoVarCo cv)          = parenSymOcc (getOccName cv) (ppr cv)

ppr_co p (TcAxiomInstCo con ind cos)
  = pprPrefixApp p (ppr (getName con) <> brackets (ppr ind)) (map pprParendTcCo cos)

ppr_co p (TcTransCo co1 co2) = maybeParen p FunPrec $
                               ppr_co FunPrec co1
                               <+> ptext (sLit ";")
                               <+> ppr_co FunPrec co2
ppr_co p (TcPhantomCo t1 t2)  = pprPrefixApp p (ptext (sLit "PhantomCo")) [pprParendType t1, pprParendType t2]
ppr_co p (TcSymCo co)         = pprPrefixApp p (ptext (sLit "Sym")) [pprParendTcCo co]
ppr_co p (TcNthCo n co)       = pprPrefixApp p (ptext (sLit "Nth:") <+> int n) [pprParendTcCo co]
ppr_co p (TcLRCo lr co)       = pprPrefixApp p (ppr lr) [pprParendTcCo co]
ppr_co p (TcSubCo co)         = pprPrefixApp p (ptext (sLit "Sub")) [pprParendTcCo co]
ppr_co p (TcAxiomRuleCo co ts ps) = maybeParen p TopPrec
                                  $ ppr_tc_axiom_rule_co co ts ps

ppr_tc_axiom_rule_co :: CoAxiomRule -> [TcType] -> [TcCoercion] -> SDoc
ppr_tc_axiom_rule_co co ts ps = ppr (coaxrName co) <> ppTs ts $$ nest 2 (ppPs ps)
  where
  ppTs []   = Outputable.empty
  ppTs [t]  = ptext (sLit "@") <> ppr_type TopPrec t
  ppTs ts   = ptext (sLit "@") <>
                parens (hsep $ punctuate comma $ map pprType ts)

  ppPs []   = Outputable.empty
  ppPs [p]  = pprParendTcCo p
  ppPs (p : ps) = ptext (sLit "(") <+> pprTcCo p $$
                  vcat [ ptext (sLit ",") <+> pprTcCo q | q <- ps ] $$
                  ptext (sLit ")")

ppr_role :: Role -> SDoc
ppr_role r = underscore <> pp_role
  where pp_role = case r of
                    Nominal          -> char 'N'
                    Representational -> char 'R'
                    Phantom          -> char 'P'

ppr_fun_co :: TyPrec -> TcCoercion -> SDoc
ppr_fun_co p co = pprArrowChain p (split co)
  where
    split :: TcCoercion -> [SDoc]
    split (TcTyConAppCo _ f [arg,res])
      | f `hasKey` funTyConKey
      = ppr_co FunPrec arg : split res
    split co = [ppr_co TopPrec co]

ppr_forall_co :: TyPrec -> TcCoercion -> SDoc
ppr_forall_co p ty
  = maybeParen p FunPrec $
    sep [pprForAll tvs, ppr_co TopPrec rho]
  where
    (tvs,  rho) = split1 [] ty
    split1 tvs (TcForAllCo tv ty) = split1 (tv:tvs) ty
    split1 tvs ty                 = (reverse tvs, ty)
\end{code}

Conversion from Coercion to TcCoercion
(at the moment, this is only needed to convert the result of
instNewTyConTF_maybe, so all unused cases are panics for now).

\begin{code}
coercionToTcCoercion :: C.Coercion -> TcCoercion
coercionToTcCoercion = go
  where
    go (C.Refl r t)                = TcRefl r t
    go (C.TransCo c1 c2)           = TcTransCo (go c1) (go c2)
    go (C.AxiomInstCo coa ind cos) = TcAxiomInstCo coa ind (map go cos)
    go (C.SubCo c)                 = TcSubCo (go c)
    go (C.AppCo c1 c2)             = TcAppCo (go c1) (go c2)
    go co                          = pprPanic "coercionToTcCoercion" (ppr co)
\end{code}


%************************************************************************
%*                                                                      *
                  HsWrapper
%*                                                                      *
%************************************************************************

\begin{code}
data HsWrapper
  = WpHole                      -- The identity coercion

  | WpCompose HsWrapper HsWrapper
       -- (wrap1 `WpCompose` wrap2)[e] = wrap1[ wrap2[ e ]]
       --
       -- Hence  (\a. []) `WpCompose` (\b. []) = (\a b. [])
       -- But    ([] a)   `WpCompose` ([] b)   = ([] b a)

  | WpCast TcCoercion         -- A cast:  [] `cast` co
                              -- Guaranteed not the identity coercion
                              -- At role Representational

        -- Evidence abstraction and application
        -- (both dictionaries and coercions)
  | WpEvLam EvVar               -- \d. []       the 'd' is an evidence variable
  | WpEvApp EvTerm              -- [] d         the 'd' is evidence for a constraint

        -- Kind and Type abstraction and application
  | WpTyLam TyVar       -- \a. []  the 'a' is a type/kind variable (not coercion var)
  | WpTyApp KindOrType  -- [] t    the 't' is a type (not coercion)


  | WpLet TcEvBinds             -- Non-empty (or possibly non-empty) evidence bindings,
                                -- so that the identity coercion is always exactly WpHole
  deriving (Data.Data, Data.Typeable)


(<.>) :: HsWrapper -> HsWrapper -> HsWrapper
WpHole <.> c = c
c <.> WpHole = c
c1 <.> c2    = c1 `WpCompose` c2

mkWpCast :: TcCoercion -> HsWrapper
mkWpCast co = ASSERT2(tcCoercionRole co == Representational, ppr co)
              WpCast co

mkWpTyApps :: [Type] -> HsWrapper
mkWpTyApps tys = mk_co_app_fn WpTyApp tys

mkWpEvApps :: [EvTerm] -> HsWrapper
mkWpEvApps args = mk_co_app_fn WpEvApp args

mkWpEvVarApps :: [EvVar] -> HsWrapper
mkWpEvVarApps vs = mkWpEvApps (map EvId vs)

mkWpTyLams :: [TyVar] -> HsWrapper
mkWpTyLams ids = mk_co_lam_fn WpTyLam ids

mkWpLams :: [Var] -> HsWrapper
mkWpLams ids = mk_co_lam_fn WpEvLam ids

mkWpLet :: TcEvBinds -> HsWrapper
-- This no-op is a quite a common case
mkWpLet (EvBinds b) | isEmptyBag b = WpHole
mkWpLet ev_binds                   = WpLet ev_binds

mk_co_lam_fn :: (a -> HsWrapper) -> [a] -> HsWrapper
mk_co_lam_fn f as = foldr (\x wrap -> f x <.> wrap) WpHole as

mk_co_app_fn :: (a -> HsWrapper) -> [a] -> HsWrapper
-- For applications, the *first* argument must
-- come *last* in the composition sequence
mk_co_app_fn f as = foldr (\x wrap -> wrap <.> f x) WpHole as

idHsWrapper :: HsWrapper
idHsWrapper = WpHole

isIdHsWrapper :: HsWrapper -> Bool
isIdHsWrapper WpHole = True
isIdHsWrapper _      = False
\end{code}


%************************************************************************
%*                                                                      *
                  Evidence bindings
%*                                                                      *
%************************************************************************

\begin{code}
data TcEvBinds
  = TcEvBinds           -- Mutable evidence bindings
       EvBindsVar       -- Mutable because they are updated "later"
                        --    when an implication constraint is solved

  | EvBinds             -- Immutable after zonking
       (Bag EvBind)

  deriving( Data.Typeable )

data EvBindsVar = EvBindsVar (IORef EvBindMap) Unique
     -- The Unique is only for debug printing

instance Data.Data TcEvBinds where
  -- Placeholder; we can't travers into TcEvBinds
  toConstr _   = abstractConstr "TcEvBinds"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = Data.mkNoRepType "TcEvBinds"

-----------------
newtype EvBindMap 
  = EvBindMap { 
       ev_bind_varenv :: VarEnv EvBind
    }       -- Map from evidence variables to evidence terms

emptyEvBindMap :: EvBindMap
emptyEvBindMap = EvBindMap { ev_bind_varenv = emptyVarEnv }

extendEvBinds :: EvBindMap -> EvVar -> EvTerm -> EvBindMap
extendEvBinds bs v t 
  = EvBindMap { ev_bind_varenv = extendVarEnv (ev_bind_varenv bs) v (EvBind v t) }

lookupEvBind :: EvBindMap -> EvVar -> Maybe EvBind
lookupEvBind bs = lookupVarEnv (ev_bind_varenv bs)

evBindMapBinds :: EvBindMap -> Bag EvBind
evBindMapBinds bs 
  = foldVarEnv consBag emptyBag (ev_bind_varenv bs)

-----------------
-- All evidence is bound by EvBinds; no side effects
data EvBind = EvBind EvVar EvTerm

data EvTerm
  = EvId EvId                    -- Any sort of evidence Id, including coercions

  | EvCoercion TcCoercion        -- (Boxed) coercion bindings
                                 -- See Note [Coercion evidence terms]

  | EvCast EvTerm TcCoercion     -- d |> co, the coercion being at role representational

  | EvDFunApp DFunId             -- Dictionary instance application
       [Type] [EvTerm]

  | EvTupleSel EvTerm  Int       -- n'th component of the tuple, 0-indexed

  | EvTupleMk [EvTerm]           -- tuple built from this stuff

  | EvDelayedError Type FastString  -- Used with Opt_DeferTypeErrors
                               -- See Note [Deferring coercion errors to runtime]
                               -- in TcSimplify

  | EvSuperClass EvTerm Int      -- n'th superclass. Used for both equalities and
                                 -- dictionaries, even though the former have no
                                 -- selector Id.  We count up from _0_

  | EvLit EvLit       -- Dictionary for KnownNat and KnownSymbol classes.
                      -- Note [KnownNat & KnownSymbol and EvLit]

  deriving( Data.Data, Data.Typeable)


data EvLit
  = EvNum Integer
  | EvStr FastString
    deriving( Data.Data, Data.Typeable)
\end{code}

Note [Coercion evidence terms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "coercion evidence term" takes one of these forms
   co_tm ::= EvId v           where v :: t1 ~ t2
           | EvCoercion co
           | EvCast co_tm co

We do quite often need to get a TcCoercion from an EvTerm; see
'evTermCoercion'.

INVARIANT: The evidence for any constraint with type (t1~t2) is 
a coercion evidence term.  Consider for example
    [G] d :: F Int a
If we have
    ax7 a :: F Int a ~ (a ~ Bool)
then we do NOT generate the constraint
    [G] (d |> ax7 a) :: a ~ Bool
because that does not satisfy the invariant (d is not a coercion variable).  
Instead we make a binding
    g1 :: a~Bool = g |> ax7 a
and the constraint
    [G] g1 :: a~Bool
See Trac [7238] and Note [Bind new Givens immediately] in TcSMonad

Note [EvBinds/EvTerm]
~~~~~~~~~~~~~~~~~~~~~
How evidence is created and updated. Bindings for dictionaries,
and coercions and implicit parameters are carried around in TcEvBinds
which during constraint generation and simplification is always of the
form (TcEvBinds ref). After constraint simplification is finished it
will be transformed to t an (EvBinds ev_bag).

Evidence for coercions *SHOULD* be filled in using the TcEvBinds
However, all EvVars that correspond to *wanted* coercion terms in
an EvBind must be mutable variables so that they can be readily
inlined (by zonking) after constraint simplification is finished.

Conclusion: a new wanted coercion variable should be made mutable.
[Notice though that evidence variables that bind coercion terms
 from super classes will be "given" and hence rigid]


Note [KnownNat & KnownSymbol and EvLit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A part of the type-level literals implementation are the classes
"KnownNat" and "KnownSymbol", which provide a "smart" constructor for
defining singleton values.  Here is the key stuff from GHC.TypeLits

  class KnownNat (n :: Nat) where
    natSing :: SNat n

  newtype SNat (n :: Nat) = SNat Integer

Conceptually, this class has infinitely many instances:

  instance KnownNat 0       where natSing = SNat 0
  instance KnownNat 1       where natSing = SNat 1
  instance KnownNat 2       where natSing = SNat 2
  ...

In practice, we solve `KnownNat` predicates in the type-checker
(see typecheck/TcInteract.hs) because we can't have infinately many instances.
The evidence (aka "dictionary") for `KnownNat` is of the form `EvLit (EvNum n)`.

We make the following assumptions about dictionaries in GHC:
  1. The "dictionary" for classes with a single method---like `KnownNat`---is
     a newtype for the type of the method, so using a evidence amounts
     to a coercion, and
  2. Newtypes use the same representation as their definition types.

So, the evidence for `KnownNat` is just a value of the representation type,
wrapped in two newtype constructors: one to make it into a `SNat` value,
and another to make it into a `KnownNat` dictionary.

Also note that `natSing` and `SNat` are never actually exposed from the
library---they are just an implementation detail.  Instead, users see
a more convenient function, defined in terms of `natSing`:

  natVal :: KnownNat n => proxy n -> Integer

The reason we don't use this directly in the class is that it is simpler
and more efficient to pass around an integer rather than an entier function,
especialy when the `KnowNat` evidence is packaged up in an existential.

The story for kind `Symbol` is analogous:
  * class KnownSymbol
  * newtype SSymbol
  * Evidence: EvLit (EvStr n)







\begin{code}
mkEvCast :: EvTerm -> TcCoercion -> EvTerm
mkEvCast ev lco
  | ASSERT2(tcCoercionRole lco == Representational, (vcat [ptext (sLit "Coercion of wrong role passed to mkEvCast:"), ppr ev, ppr lco]))
    isTcReflCo lco = ev
  | otherwise      = EvCast ev lco

emptyTcEvBinds :: TcEvBinds
emptyTcEvBinds = EvBinds emptyBag

isEmptyTcEvBinds :: TcEvBinds -> Bool
isEmptyTcEvBinds (EvBinds b)    = isEmptyBag b
isEmptyTcEvBinds (TcEvBinds {}) = panic "isEmptyTcEvBinds"


evTermCoercion :: EvTerm -> TcCoercion
-- Applied only to EvTerms of type (s~t)
-- See Note [Coercion evidence terms]
evTermCoercion (EvId v)        = mkTcCoVarCo v
evTermCoercion (EvCoercion co) = co
evTermCoercion (EvCast tm co)  = TcCastCo (evTermCoercion tm) co
evTermCoercion tm = pprPanic "evTermCoercion" (ppr tm)

evVarsOfTerm :: EvTerm -> VarSet
evVarsOfTerm (EvId v)             = unitVarSet v
evVarsOfTerm (EvCoercion co)      = coVarsOfTcCo co
evVarsOfTerm (EvDFunApp _ _ evs)  = evVarsOfTerms evs
evVarsOfTerm (EvTupleSel v _)     = evVarsOfTerm v
evVarsOfTerm (EvSuperClass v _)   = evVarsOfTerm v
evVarsOfTerm (EvCast tm co)       = evVarsOfTerm tm `unionVarSet` coVarsOfTcCo co
evVarsOfTerm (EvTupleMk evs)      = evVarsOfTerms evs
evVarsOfTerm (EvDelayedError _ _) = emptyVarSet
evVarsOfTerm (EvLit _)            = emptyVarSet

evVarsOfTerms :: [EvTerm] -> VarSet
evVarsOfTerms = foldr (unionVarSet . evVarsOfTerm) emptyVarSet 
\end{code}


%************************************************************************
%*                                                                      *
                  Pretty printing
%*                                                                      *
%************************************************************************

\begin{code}
instance Outputable HsWrapper where
  ppr co_fn = pprHsWrapper (ptext (sLit "<>")) co_fn

pprHsWrapper :: SDoc -> HsWrapper -> SDoc
-- In debug mode, print the wrapper
-- otherwise just print what's inside
pprHsWrapper doc wrap
  = getPprStyle (\ s -> if debugStyle s then (help (add_parens doc) wrap False) else doc)
  where
    help :: (Bool -> SDoc) -> HsWrapper -> Bool -> SDoc
    -- True  <=> appears in function application position
    -- False <=> appears as body of let or lambda
    help it WpHole             = it
    help it (WpCompose f1 f2)  = help (help it f2) f1
    help it (WpCast co)   = add_parens $ sep [it False, nest 2 (ptext (sLit "|>")
                                              <+> pprParendTcCo co)]
    help it (WpEvApp id)  = no_parens  $ sep [it True, nest 2 (ppr id)]
    help it (WpTyApp ty)  = no_parens  $ sep [it True, ptext (sLit "@") <+> pprParendType ty]
    help it (WpEvLam id)  = add_parens $ sep [ ptext (sLit "\\") <> pp_bndr id, it False]
    help it (WpTyLam tv)  = add_parens $ sep [ptext (sLit "/\\") <> pp_bndr tv, it False]
    help it (WpLet binds) = add_parens $ sep [ptext (sLit "let") <+> braces (ppr binds), it False]

    pp_bndr v = pprBndr LambdaBind v <> dot

    add_parens, no_parens :: SDoc -> Bool -> SDoc
    add_parens d True  = parens d
    add_parens d False = d
    no_parens d _ = d

instance Outputable TcEvBinds where
  ppr (TcEvBinds v) = ppr v
  ppr (EvBinds bs)  = ptext (sLit "EvBinds") <> braces (vcat (map ppr (bagToList bs)))

instance Outputable EvBindsVar where
  ppr (EvBindsVar _ u) = ptext (sLit "EvBindsVar") <> angleBrackets (ppr u)

instance Outputable EvBind where
  ppr (EvBind v e)   = sep [ ppr v, nest 2 $ equals <+> ppr e ]
   -- We cheat a bit and pretend EqVars are CoVars for the purposes of pretty printing

instance Outputable EvTerm where
  ppr (EvId v)           = ppr v
  ppr (EvCast v co)      = ppr v <+> (ptext (sLit "`cast`")) <+> pprParendTcCo co
  ppr (EvCoercion co)    = ptext (sLit "CO") <+> ppr co
  ppr (EvTupleSel v n)   = ptext (sLit "tupsel") <> parens (ppr (v,n))
  ppr (EvTupleMk vs)     = ptext (sLit "tupmk") <+> ppr vs
  ppr (EvSuperClass d n) = ptext (sLit "sc") <> parens (ppr (d,n))
  ppr (EvDFunApp df tys ts) = ppr df <+> sep [ char '@' <> ppr tys, ppr ts ]
  ppr (EvLit l)          = ppr l
  ppr (EvDelayedError ty msg) =     ptext (sLit "error") 
                                <+> sep [ char '@' <> ppr ty, ppr msg ]

instance Outputable EvLit where
  ppr (EvNum n) = integer n
  ppr (EvStr s) = text (show s)
\end{code}


-- (c) The University of Glasgow 2006

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module TcEvidence (

  -- HsWrapper
  HsWrapper(..),
  (<.>), mkWpTyApps, mkWpEvApps, mkWpEvVarApps, mkWpTyLams, mkWpLams, mkWpLet, mkWpCast,
  mkWpFun, idHsWrapper, isIdHsWrapper, pprHsWrapper,

  -- Evidence bindings
  TcEvBinds(..), EvBindsVar(..),
  EvBindMap(..), emptyEvBindMap, extendEvBinds,
                 lookupEvBind, evBindMapBinds, foldEvBindMap,
  EvBind(..), emptyTcEvBinds, isEmptyTcEvBinds, mkGivenEvBind, mkWantedEvBind,
  EvTerm(..), mkEvCast, evVarsOfTerm, mkEvScSelectors,
  EvLit(..), evTermCoercion,
  EvCallStack(..),
  EvTypeable(..),

  -- TcCoercion
  TcCoercion(..), TcCoercionR, TcCoercionN,
  LeftOrRight(..), pickLR,
  mkTcReflCo, mkTcNomReflCo, mkTcRepReflCo,
  mkTcTyConAppCo, mkTcAppCo, mkTcAppCos, mkTcFunCo,
  mkTcAxInstCo, mkTcUnbranchedAxInstCo, mkTcForAllCo, mkTcForAllCos,
  mkTcSymCo, mkTcTransCo, mkTcNthCo, mkTcLRCo, mkTcSubCo, maybeTcSubCo,
  tcDowngradeRole, mkTcTransAppCo,
  mkTcAxiomRuleCo, mkTcPhantomCo,
  tcCoercionKind, coVarsOfTcCo, isEqVar, mkTcCoVarCo,
  isTcReflCo, getTcCoVar_maybe,
  tcCoercionRole, eqVarRole,
  unwrapIP, wrapIP
  ) where
#include "HsVersions.h"

import Var
import Coercion
import PprCore ()   -- Instance OutputableBndr TyVar
import TypeRep      -- Knows type representation
import TcType
import Type
import TyCon
import Class( Class )
import CoAxiom
import PrelNames
import VarEnv
import VarSet
import Name

import Util
import Bag
import Pair
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
import Data.Traversable (traverse, sequenceA)
#endif
import qualified Data.Data as Data
import Outputable
import FastString
import SrcLoc
import Data.IORef( IORef )

{-
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

Note [mkTcTransAppCo]
~~~~~~~~~~~~~~~~~~~~~
Suppose we have

  co1 :: a ~R Maybe
  co2 :: b ~R Int

and we want

  co3 :: a b ~R Maybe Int

This seems sensible enough. But, we can't let (co3 = co1 co2), because
that's ill-roled! Note that mkTcAppCo requires a *nominal* second coercion.

The way around this is to use transitivity:

  co3 = (co1 <b>_N) ; (Maybe co2) :: a b ~R Maybe Int

Or, it's possible everything is the other way around:

  co1' :: Maybe ~R a
  co2' :: Int   ~R b

and we want

  co3' :: Maybe Int ~R a b

then

  co3' = (Maybe co2') ; (co1' <b>_N)

This is exactly what `mkTcTransAppCo` builds for us. Information for all
the arguments tends to be to hand at call sites, so it's quicker than
using, say, tcCoercionKind.
-}

type TcCoercionN = TcCoercion    -- A Nominal          corecion ~N
type TcCoercionR = TcCoercion    -- A Representational corecion ~R

data TcCoercion
  = TcRefl Role TcType
  | TcTyConAppCo Role TyCon [TcCoercion]
  | TcAppCo TcCoercion TcCoercion
  | TcForAllCo TyVar TcCoercion
  | TcCoVarCo EqVar
  | TcAxiomInstCo (CoAxiom Branched) Int -- Int specifies branch number
                  [TcCoercion]           -- See [CoAxiom Index] in Coercion.hs
  -- This is number of types and coercions are expected to match to CoAxiomRule
  -- (i.e., the CoAxiomRules are always fully saturated)
  | TcAxiomRuleCo CoAxiomRule [TcType] [TcCoercion]
  | TcPhantomCo TcType TcType
  | TcSymCo TcCoercion
  | TcTransCo TcCoercion TcCoercion
  | TcNthCo Int TcCoercion
  | TcLRCo LeftOrRight TcCoercion
  | TcSubCo TcCoercion                 -- Argument is never TcRefl
  | TcCastCo TcCoercion TcCoercion     -- co1 |> co2
  | TcLetCo TcEvBinds TcCoercion
  | TcCoercion Coercion            -- embed a Core Coercion
  deriving (Data.Data, Data.Typeable)

isEqVar :: Var -> Bool
-- Is lifted coercion variable (only!)
isEqVar v = case tyConAppTyCon_maybe (varType v) of
               Just tc -> tc `hasKey` eqTyConKey
               Nothing -> False

isTcReflCo_maybe :: TcCoercion -> Maybe TcType
isTcReflCo_maybe (TcRefl _ ty)   = Just ty
isTcReflCo_maybe (TcCoercion co) = isReflCo_maybe co
isTcReflCo_maybe _               = Nothing

isTcReflCo :: TcCoercion -> Bool
isTcReflCo (TcRefl {})     = True
isTcReflCo (TcCoercion co) = isReflCo co
isTcReflCo _               = False

getTcCoVar_maybe :: TcCoercion -> Maybe CoVar
getTcCoVar_maybe (TcCoVarCo v) = Just v
getTcCoVar_maybe _             = Nothing

mkTcReflCo :: Role -> TcType -> TcCoercion
mkTcReflCo = TcRefl

mkTcNomReflCo :: TcType -> TcCoercion
mkTcNomReflCo = TcRefl Nominal

mkTcRepReflCo :: TcType -> TcCoercion
mkTcRepReflCo = TcRefl Representational

mkTcFunCo :: Role -> TcCoercion -> TcCoercion -> TcCoercion
mkTcFunCo role co1 co2 = mkTcTyConAppCo role funTyCon [co1, co2]

mkTcTyConAppCo :: Role -> TyCon -> [TcCoercion] -> TcCoercion
mkTcTyConAppCo role tc cos -- No need to expand type synonyms
                           -- See Note [TcCoercions]
  | Just tys <- traverse isTcReflCo_maybe cos
  = TcRefl role (mkTyConApp tc tys)  -- See Note [Refl invariant]

  | otherwise = TcTyConAppCo role tc cos

-- Input coercion is Nominal
-- mkSubCo will do some normalisation. We do not do it for TcCoercions, but
-- defer that to desugaring; just to reduce the code duplication a little bit
mkTcSubCo :: TcCoercion -> TcCoercion
mkTcSubCo (TcRefl _ ty)
  = TcRefl Representational ty
mkTcSubCo co
   = ASSERT2( tcCoercionRole co == Nominal, ppr co)
     TcSubCo co

-- See Note [Role twiddling functions] in Coercion
-- | Change the role of a 'TcCoercion'. Returns 'Nothing' if this isn't
-- a downgrade.
tcDowngradeRole_maybe :: Role   -- desired role
                      -> Role   -- current role
                      -> TcCoercion -> Maybe TcCoercion
tcDowngradeRole_maybe Representational Nominal = Just . mkTcSubCo
tcDowngradeRole_maybe Nominal Representational = const Nothing
tcDowngradeRole_maybe Phantom _
  = panic "tcDowngradeRole_maybe Phantom"
    -- not supported (not needed at the moment)
tcDowngradeRole_maybe _ Phantom                = const Nothing
tcDowngradeRole_maybe _ _                      = Just

-- See Note [Role twiddling functions] in Coercion
-- | Change the role of a 'TcCoercion'. Panics if this isn't a downgrade.
tcDowngradeRole :: Role  -- ^ desired role
                -> Role  -- ^ current role
                -> TcCoercion -> TcCoercion
tcDowngradeRole r1 r2 co
  = case tcDowngradeRole_maybe r1 r2 co of
      Just co' -> co'
      Nothing  -> pprPanic "tcDowngradeRole" (ppr r1 <+> ppr r2 <+> ppr co)

-- | If the EqRel is ReprEq, makes a TcSubCo; otherwise, does nothing.
-- Note that the input coercion should always be nominal.
maybeTcSubCo :: EqRel -> TcCoercion -> TcCoercion
maybeTcSubCo NomEq  = id
maybeTcSubCo ReprEq = mkTcSubCo

mkTcAxInstCo :: Role -> CoAxiom br -> Int -> [TcType] -> TcCoercion
mkTcAxInstCo role ax index tys
  | ASSERT2( not (role == Nominal && ax_role == Representational) , ppr (ax, tys) )
    arity == n_tys = tcDowngradeRole role ax_role $
                     TcAxiomInstCo ax_br index rtys
  | otherwise      = ASSERT( arity < n_tys )
                     tcDowngradeRole role ax_role $
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
-- Second coercion *must* be nominal
mkTcAppCo (TcRefl r ty1) (TcRefl _ ty2) = TcRefl r (mkAppTy ty1 ty2)
mkTcAppCo co1 co2                       = TcAppCo co1 co2

-- | Like `mkTcAppCo`, but allows the second coercion to be other than
-- nominal. See Note [mkTcTransAppCo]. Role r3 cannot be more stringent
-- than either r1 or r2.
mkTcTransAppCo :: Role           -- ^ r1
               -> TcCoercion     -- ^ co1 :: ty1a ~r1 ty1b
               -> TcType         -- ^ ty1a
               -> TcType         -- ^ ty1b
               -> Role           -- ^ r2
               -> TcCoercion     -- ^ co2 :: ty2a ~r2 ty2b
               -> TcType         -- ^ ty2a
               -> TcType         -- ^ ty2b
               -> Role           -- ^ r3
               -> TcCoercion     -- ^ :: ty1a ty2a ~r3 ty1b ty2b
mkTcTransAppCo r1 co1 ty1a ty1b r2 co2 ty2a ty2b r3
-- How incredibly fiddly! Is there a better way??
  = case (r1, r2, r3) of
      (_,                _,                Phantom)
        -> mkTcPhantomCo (mkAppTy ty1a ty2a) (mkAppTy ty1b ty2b)
      (_,                _,                Nominal)
        -> ASSERT( r1 == Nominal && r2 == Nominal )
           mkTcAppCo co1 co2
      (Nominal,          Nominal,          Representational)
        -> mkTcSubCo (mkTcAppCo co1 co2)
      (_,                Nominal,          Representational)
        -> ASSERT( r1 == Representational )
           mkTcAppCo co1 co2
      (Nominal,          Representational, Representational)
        -> go (mkTcSubCo co1)
      (_               , _,                Representational)
        -> ASSERT( r1 == Representational && r2 == Representational )
           go co1
  where
    go co1_repr
      | Just (tc1b, tys1b) <- tcSplitTyConApp_maybe ty1b
      , nextRole ty1b == r2
      = (co1_repr `mkTcAppCo` mkTcNomReflCo ty2a) `mkTcTransCo`
        (mkTcTyConAppCo Representational tc1b
           (zipWith mkTcReflCo (tyConRolesX Representational tc1b) tys1b
            ++ [co2]))

      | Just (tc1a, tys1a) <- tcSplitTyConApp_maybe ty1a
      , nextRole ty1a == r2
      = (mkTcTyConAppCo Representational tc1a
           (zipWith mkTcReflCo (tyConRolesX Representational tc1a) tys1a
            ++ [co2]))
        `mkTcTransCo`
        (co1_repr `mkTcAppCo` mkTcNomReflCo ty2b)

      | otherwise
      = pprPanic "mkTcTransAppCo" (vcat [ ppr r1, ppr co1, ppr ty1a, ppr ty1b
                                        , ppr r2, ppr co2, ppr ty2a, ppr ty2b
                                        , ppr r3 ])

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

mkTcPhantomCo :: TcType -> TcType -> TcCoercion
mkTcPhantomCo = TcPhantomCo

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
    go (TcCoercion co)        = coercionKind co

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
    go (TcCoercion co)        = coercionRole co


coVarsOfTcCo :: TcCoercion -> VarSet
-- Only works on *zonked* coercions, because of TcLetCo
coVarsOfTcCo tc_co
  = go tc_co
  where
    go (TcRefl _ _)              = emptyVarSet
    go (TcTyConAppCo _ _ cos)    = mapUnionVarSet go cos
    go (TcAppCo co1 co2)         = go co1 `unionVarSet` go co2
    go (TcCastCo co1 co2)        = go co1 `unionVarSet` go co2
    go (TcForAllCo _ co)         = go co
    go (TcCoVarCo v)             = unitVarSet v
    go (TcAxiomInstCo _ _ cos)   = mapUnionVarSet go cos
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
    go (TcAxiomRuleCo _ _ cos)   = mapUnionVarSet go cos
    go (TcCoercion co)           = -- the use of coVarsOfTcCo in dsTcCoercion will
                                   -- fail if there are any proper, unlifted covars
                                   ASSERT( isEmptyVarSet (coVarsOfCo co) )
                                   emptyVarSet

    -- We expect only coercion bindings, so use evTermCoercion
    go_bind :: EvBind -> VarSet
    go_bind (EvBind { eb_rhs =tm }) = go (evTermCoercion tm)

    get_bndrs :: Bag EvBind -> VarSet
    get_bndrs = foldrBag (\ (EvBind { eb_lhs = b }) bs -> extendVarSet bs b) emptyVarSet

-- Pretty printing

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
ppr_co p (TcCoercion co)      = pprPrefixApp p (text "Core co:") [ppr co]

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

{-
************************************************************************
*                                                                      *
                  HsWrapper
*                                                                      *
************************************************************************
-}

data HsWrapper
  = WpHole                      -- The identity coercion

  | WpCompose HsWrapper HsWrapper
       -- (wrap1 `WpCompose` wrap2)[e] = wrap1[ wrap2[ e ]]
       --
       -- Hence  (\a. []) `WpCompose` (\b. []) = (\a b. [])
       -- But    ([] a)   `WpCompose` ([] b)   = ([] b a)

  | WpFun HsWrapper HsWrapper TcType TcType
       -- (WpFun wrap1 wrap2 t1 t2)[e] = \(x:t1). wrap2[ e wrap1[x] ] :: t2
       -- So note that if  wrap1 :: exp_arg <= act_arg
       --                  wrap2 :: act_res <= exp_res
       --           then   WpFun wrap1 wrap2 : (act_arg -> arg_res) <= (exp_arg -> exp_res)
       -- This isn't the same as for mkTcFunCo, but it has to be this way
       -- because we can't use 'sym' to flip around these HsWrappers

  | WpCast TcCoercion         -- A cast:  [] `cast` co
                              -- Guaranteed not the identity coercion
                              -- At role Representational

        -- Evidence abstraction and application
        -- (both dictionaries and coercions)
  | WpEvLam  EvVar               -- \d. []       the 'd' is an evidence variable
  | WpEvApp  EvTerm              -- [] d         the 'd' is evidence for a constraint

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

mkWpFun :: HsWrapper -> HsWrapper -> TcType -> TcType -> HsWrapper
mkWpFun WpHole       WpHole       _  _  = WpHole
mkWpFun WpHole       (WpCast co2) t1 _  = WpCast (mkTcFunCo Representational (mkTcRepReflCo t1) co2)
mkWpFun (WpCast co1) WpHole       _  t2 = WpCast (mkTcFunCo Representational (mkTcSymCo co1) (mkTcRepReflCo t2))
mkWpFun (WpCast co1) (WpCast co2) _  _  = WpCast (mkTcFunCo Representational (mkTcSymCo co1) co2)
mkWpFun co1          co2          t1 t2 = WpFun co1 co2 t1 t2

mkWpCast :: TcCoercion -> HsWrapper
mkWpCast co
  | isTcReflCo co = WpHole
  | otherwise     = ASSERT2(tcCoercionRole co == Representational, ppr co)
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

{-
************************************************************************
*                                                                      *
                  Evidence bindings
*                                                                      *
************************************************************************
-}

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

extendEvBinds :: EvBindMap -> EvBind -> EvBindMap
extendEvBinds bs ev_bind
  = EvBindMap { ev_bind_varenv = extendVarEnv (ev_bind_varenv bs)
                                              (eb_lhs ev_bind)
                                              ev_bind }

lookupEvBind :: EvBindMap -> EvVar -> Maybe EvBind
lookupEvBind bs = lookupVarEnv (ev_bind_varenv bs)

evBindMapBinds :: EvBindMap -> Bag EvBind
evBindMapBinds = foldEvBindMap consBag emptyBag

foldEvBindMap :: (EvBind -> a -> a) -> a -> EvBindMap -> a
foldEvBindMap k z bs = foldVarEnv k z (ev_bind_varenv bs)

-----------------
-- All evidence is bound by EvBinds; no side effects
data EvBind
  = EvBind { eb_lhs      :: EvVar
           , eb_rhs      :: EvTerm
           , eb_is_given :: Bool  -- True <=> given
                 -- See Note [Tracking redundant constraints] in TcSimplify
    }

mkWantedEvBind :: EvVar -> EvTerm -> EvBind
mkWantedEvBind ev tm = EvBind { eb_is_given = False, eb_lhs = ev, eb_rhs = tm }

mkGivenEvBind :: EvVar -> EvTerm -> EvBind
mkGivenEvBind ev tm = EvBind { eb_is_given = True, eb_lhs = ev, eb_rhs = tm }

data EvTerm
  = EvId EvId                    -- Any sort of evidence Id, including coercions

  | EvCoercion TcCoercion        -- (Boxed) coercion bindings
                                 -- See Note [Coercion evidence terms]

  | EvCast EvTerm TcCoercion     -- d |> co, the coercion being at role representational

  | EvDFunApp DFunId             -- Dictionary instance application
       [Type] [EvId]

  | EvDelayedError Type FastString  -- Used with Opt_DeferTypeErrors
                               -- See Note [Deferring coercion errors to runtime]
                               -- in TcSimplify

  | EvSuperClass EvTerm Int      -- n'th superclass. Used for both equalities and
                                 -- dictionaries, even though the former have no
                                 -- selector Id.  We count up from _0_

  | EvLit EvLit       -- Dictionary for KnownNat and KnownSymbol classes.
                      -- Note [KnownNat & KnownSymbol and EvLit]

  | EvCallStack EvCallStack -- Dictionary for CallStack implicit parameters

  | EvTypeable EvTypeable   -- Dictionary for `Typeable`

  deriving( Data.Data, Data.Typeable )


-- | Instructions on how to make a 'Typeable' dictionary.
data EvTypeable
  = EvTypeableTyCon TyCon [Kind]
    -- ^ Dictionary for concrete type constructors.

  | EvTypeableTyApp (EvTerm,Type) (EvTerm,Type)
    -- ^ Dictionary for type applications;  this is used when we have
    -- a type expression starting with a type variable (e.g., @Typeable (f a)@)

  | EvTypeableTyLit (EvTerm,Type)
    -- ^ Dictionary for a type literal.

  deriving ( Data.Data, Data.Typeable )

data EvLit
  = EvNum Integer
  | EvStr FastString
    deriving( Data.Data, Data.Typeable )

-- | Evidence for @CallStack@ implicit parameters.
data EvCallStack
  -- See Note [Overview of implicit CallStacks]
  = EvCsEmpty
  | EvCsPushCall Name RealSrcSpan EvTerm
    -- ^ @EvCsPushCall name loc stk@ represents a call to @name@, occurring at
    -- @loc@, in a calling context @stk@.
  | EvCsTop FastString RealSrcSpan EvTerm
    -- ^ @EvCsTop name loc stk@ represents a use of an implicit parameter
    -- @?name@, occurring at @loc@, in a calling context @stk@.
  deriving( Data.Data, Data.Typeable )

{-
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
See Trac [7238] and Note [Bind new Givens immediately] in TcRnTypes

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


Note [Overview of implicit CallStacks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(See https://ghc.haskell.org/trac/ghc/wiki/ExplicitCallStack/ImplicitLocations)

The goal of CallStack evidence terms is to reify locations
in the program source as runtime values, without any support
from the RTS. We accomplish this by assigning a special meaning
to implicit parameters of type GHC.Stack.CallStack. A use of
a CallStack IP, e.g.

  head []    = error (show (?loc :: CallStack))
  head (x:_) = x

will be solved with the source location that gave rise to the IP
constraint (here, the use of ?loc). If there is already
a CallStack IP in scope, e.g. passed-in as an argument

  head :: (?loc :: CallStack) => [a] -> a
  head []    = error (show (?loc :: CallStack))
  head (x:_) = x

we will push the new location onto the CallStack that was passed
in. These two cases are reflected by the EvCallStack evidence
type. In the first case, we will create an evidence term

  EvCsTop "?loc" <?loc's location> EvCsEmpty

and in the second we'll have a given constraint

  [G] d :: IP "loc" CallStack

in scope, and will create an evidence term

  EvCsTop "?loc" <?loc's location> d

When we call a function that uses a CallStack IP, e.g.

  f = head xs

we create an evidence term

  EvCsPushCall "head" <head's location> EvCsEmpty

again pushing onto a given evidence term if one exists.

This provides a lightweight mechanism for building up call-stacks
explicitly, but is notably limited by the fact that the stack will
stop at the first function whose type does not include a CallStack IP.
For example, using the above definition of head:

  f :: [a] -> a
  f = head

  g = f []

the resulting CallStack will include use of ?loc inside head and
the call to head inside f, but NOT the call to f inside g, because f
did not explicitly request a CallStack.

Important Details:
- GHC should NEVER report an insoluble CallStack constraint.

- A CallStack (defined in GHC.Stack) is a [(String, SrcLoc)], where the String
  is the name of the binder that is used at the SrcLoc. SrcLoc is defined in
  GHC.SrcLoc and contains the package/module/file name, as well as the full
  source-span. Both CallStack and SrcLoc are kept abstract so only GHC can
  construct new values.

- Consider the use of ?stk in:

    head :: (?stk :: CallStack) => [a] -> a
    head [] = error (show ?stk)

  When solving the use of ?stk we'll have a given

   [G] d :: IP "stk" CallStack

  in scope. In the interaction phase, GHC would normally solve the use of ?stk
  directly from the given, i.e. re-using the dicionary. But this is NOT what we
  want! We want to generate a *new* CallStack with ?loc's SrcLoc pushed onto
  the given CallStack. So we must take care in TcInteract.interactDict to
  prioritize solving wanted CallStacks.

- We will automatically solve any wanted CallStack regardless of the name of the
  IP, i.e.

    f = show (?stk :: CallStack)
    g = show (?loc :: CallStack)

  are both valid. However, we will only push new SrcLocs onto existing
  CallStacks when the IP names match, e.g. in

    head :: (?loc :: CallStack) => [a] -> a
    head [] = error (show (?stk :: CallStack))

  the printed CallStack will NOT include head's call-site. This reflects the
  standard scoping rules of implicit-parameters. (See TcInteract.interactDict)

- An EvCallStack term desugars to a CoreExpr of type `IP "some str" CallStack`.
  The desugarer will need to unwrap the IP newtype before pushing a new
  call-site onto a given stack (See DsBinds.dsEvCallStack)

- We only want to intercept constraints that arose due to the use of an IP or a
  function call. In particular, we do NOT want to intercept the

    (?stk :: CallStack) => [a] -> a
      ~
    (?stk :: CallStack) => [a] -> a

  constraint that arises from the ambiguity check on `head`s type signature.
  (See TcEvidence.isCallStackIP)
-}

mkEvCast :: EvTerm -> TcCoercion -> EvTerm
mkEvCast ev lco
  | ASSERT2(tcCoercionRole lco == Representational, (vcat [ptext (sLit "Coercion of wrong role passed to mkEvCast:"), ppr ev, ppr lco]))
    isTcReflCo lco = ev
  | otherwise      = EvCast ev lco

mkEvScSelectors :: EvTerm -> Class -> [TcType] -> [(TcPredType, EvTerm)]
mkEvScSelectors ev cls tys
   = zipWith mk_pr (immSuperClasses cls tys) [0..]
  where
    mk_pr pred i = (pred, EvSuperClass ev i)

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
evVarsOfTerm (EvDFunApp _ _ evs)  = mkVarSet evs
evVarsOfTerm (EvSuperClass v _)   = evVarsOfTerm v
evVarsOfTerm (EvCast tm co)       = evVarsOfTerm tm `unionVarSet` coVarsOfTcCo co
evVarsOfTerm (EvDelayedError _ _) = emptyVarSet
evVarsOfTerm (EvLit _)            = emptyVarSet
evVarsOfTerm (EvCallStack cs)     = evVarsOfCallStack cs
evVarsOfTerm (EvTypeable ev)      = evVarsOfTypeable ev

evVarsOfTerms :: [EvTerm] -> VarSet
evVarsOfTerms = mapUnionVarSet evVarsOfTerm

evVarsOfCallStack :: EvCallStack -> VarSet
evVarsOfCallStack cs = case cs of
  EvCsEmpty -> emptyVarSet
  EvCsTop _ _ tm -> evVarsOfTerm tm
  EvCsPushCall _ _ tm -> evVarsOfTerm tm

evVarsOfTypeable :: EvTypeable -> VarSet
evVarsOfTypeable ev =
  case ev of
    EvTypeableTyCon _ _    -> emptyVarSet
    EvTypeableTyApp e1 e2  -> evVarsOfTerms (map fst [e1,e2])
    EvTypeableTyLit e      -> evVarsOfTerm (fst e)

{-
************************************************************************
*                                                                      *
                  Pretty printing
*                                                                      *
************************************************************************
-}

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
    help it (WpFun f1 f2 t1 _) = add_parens $ ptext (sLit "\\(x") <> dcolon <> ppr t1 <> ptext (sLit ").") <+>
                                              help (\_ -> it True <+> help (\_ -> ptext (sLit "x")) f1 True) f2 False
    help it (WpCast co)   = add_parens $ sep [it False, nest 2 (ptext (sLit "|>")
                                              <+> pprParendTcCo co)]
    help it (WpEvApp id)    = no_parens  $ sep [it True, nest 2 (ppr id)]
    help it (WpTyApp ty)    = no_parens  $ sep [it True, ptext (sLit "@") <+> pprParendType ty]
    help it (WpEvLam id)    = add_parens $ sep [ ptext (sLit "\\") <> pp_bndr id, it False]
    help it (WpTyLam tv)    = add_parens $ sep [ptext (sLit "/\\") <> pp_bndr tv, it False]
    help it (WpLet binds)   = add_parens $ sep [ptext (sLit "let") <+> braces (ppr binds), it False]

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
  ppr (EvBind { eb_lhs = v, eb_rhs = e, eb_is_given = is_given })
     = sep [ pp_gw <+> ppr v
           , nest 2 $ equals <+> ppr e ]
     where
       pp_gw = brackets (if is_given then char 'G' else char 'W')
   -- We cheat a bit and pretend EqVars are CoVars for the purposes of pretty printing

instance Outputable EvTerm where
  ppr (EvId v)              = ppr v
  ppr (EvCast v co)         = ppr v <+> (ptext (sLit "`cast`")) <+> pprParendTcCo co
  ppr (EvCoercion co)       = ptext (sLit "CO") <+> ppr co
  ppr (EvSuperClass d n)    = ptext (sLit "sc") <> parens (ppr (d,n))
  ppr (EvDFunApp df tys ts) = ppr df <+> sep [ char '@' <> ppr tys, ppr ts ]
  ppr (EvLit l)             = ppr l
  ppr (EvCallStack cs)      = ppr cs
  ppr (EvDelayedError ty msg) =     ptext (sLit "error")
                                <+> sep [ char '@' <> ppr ty, ppr msg ]
  ppr (EvTypeable ev)    = ppr ev

instance Outputable EvLit where
  ppr (EvNum n) = integer n
  ppr (EvStr s) = text (show s)

instance Outputable EvCallStack where
  ppr EvCsEmpty
    = ptext (sLit "[]")
  ppr (EvCsTop name loc tm)
    = angleBrackets (ppr (name,loc)) <+> ptext (sLit ":") <+> ppr tm
  ppr (EvCsPushCall name loc tm)
    = angleBrackets (ppr (name,loc)) <+> ptext (sLit ":") <+> ppr tm

instance Outputable EvTypeable where
  ppr ev =
    case ev of
      EvTypeableTyCon tc ks    -> parens (ppr tc <+> sep (map ppr ks))
      EvTypeableTyApp t1 t2    -> parens (ppr (fst t1) <+> ppr (fst t2))
      EvTypeableTyLit x        -> ppr (fst x)


----------------------------------------------------------------------
-- Helper functions for dealing with IP newtype-dictionaries
----------------------------------------------------------------------

-- | Create a 'Coercion' that unwraps an implicit-parameter dictionary
-- to expose the underlying value. We expect the 'Type' to have the form
-- `IP sym ty`, return a 'Coercion' `co :: IP sym ty ~ ty`.
unwrapIP :: Type -> Coercion
unwrapIP ty =
  case unwrapNewTyCon_maybe tc of
    Just (_,_,ax) -> mkUnbranchedAxInstCo Representational ax tys
    Nothing       -> pprPanic "unwrapIP" $
                       text "The dictionary for" <+> quotes (ppr tc)
                         <+> text "is not a newtype!"
  where
  (tc, tys) = splitTyConApp ty

-- | Create a 'Coercion' that wraps a value in an implicit-parameter
-- dictionary. See 'unwrapIP'.
wrapIP :: Type -> Coercion
wrapIP ty = mkSymCo (unwrapIP ty)

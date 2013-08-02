%
% (c) The University of Glasgow 2006
%

\begin{code}
module TcEvidence (

  -- HsWrapper
  HsWrapper(..), 
  (<.>), mkWpTyApps, mkWpEvApps, mkWpEvVarApps, mkWpTyLams, mkWpLams, mkWpLet, 
  idHsWrapper, isIdHsWrapper, pprHsWrapper,

  -- Evidence bindings
  TcEvBinds(..), EvBindsVar(..), 
  EvBindMap(..), emptyEvBindMap, extendEvBinds, lookupEvBind, evBindMapBinds,
  EvBind(..), emptyTcEvBinds, isEmptyTcEvBinds, 
  EvTerm(..), mkEvCast, evVarsOfTerm, 
  EvLit(..), evTermCoercion,

  -- TcCoercion
  TcCoercion(..), LeftOrRight(..), pickLR,
  mkTcReflCo, mkTcTyConAppCo, mkTcAppCo, mkTcAppCos, mkTcFunCo,
  mkTcAxInstCo, mkTcUnbranchedAxInstCo, mkTcForAllCo, mkTcForAllCos, 
  mkTcSymCo, mkTcTransCo, mkTcNthCo, mkTcLRCo, mkTcInstCos,
  tcCoercionKind, coVarsOfTcCo, isEqVar, mkTcCoVarCo, 
  isTcReflCo, isTcReflCo_maybe, getTcCoVar_maybe,
  liftTcCoSubstWith

  ) where
#include "HsVersions.h"

import Var
import Coercion( LeftOrRight(..), pickLR )
import PprCore ()   -- Instance OutputableBndr TyVar
import TypeRep  -- Knows type representation
import TcType
import Type( tyConAppArgN, tyConAppTyCon_maybe, getEqPredTys, coAxNthLHS )
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
| LCoercions are a hack used by the typechecker. Normally,
Coercions have free variables of type (a ~# b): we call these
CoVars. However, the type checker passes around equality evidence
(boxed up) at type (a ~ b).

An LCoercion is simply a Coercion whose free variables have the
boxed type (a ~ b). After we are done with typechecking the
desugarer finds the free variables, unboxes them, and creates a
resulting real Coercion with kosher free variables.

We can use most of the Coercion "smart constructors" to build LCoercions. However,
mkCoVarCo will not work! The equivalent is mkTcCoVarCo.

The data type is similar to Coercion.Coercion, with the following
differences
  * Most important, TcLetCo adds let-bindings for coercions.
    This is what lets us unify two for-all types and generate
    equality constraints underneath

  * The kind of a TcCoercion is  t1 ~  t2 
             of a Coercion   is  t1 ~# t2

  * TcCoercions are essentially all at role Nominal -- the type-checker
    reasons only about nominal equality, not representational.
    --> Exception: there can be newtype axioms wrapped up in TcCoercions.
                   These, of course, are only used in casts, so the desugarer
                   will still produce the right 'Coercion's.

  * TcAxiomInstCo takes Types, not Coecions as arguments;
    the generality is required only in the Simplifier

  * UnsafeCo aren't required

  * Reprsentation invariants are weaker:
     - we are allowed to have type synonyms in TcTyConAppCo
     - the first arg of a TcAppCo can be a TcTyConAppCo
    Reason: they'll get established when we desugar to Coercion

\begin{code}
data TcCoercion 
  = TcRefl TcType
  | TcTyConAppCo TyCon [TcCoercion]
  | TcAppCo TcCoercion TcCoercion
  | TcForAllCo TyVar TcCoercion 
  | TcInstCo TcCoercion TcType
  | TcCoVarCo EqVar               -- variable always at role N
  | TcAxiomInstCo (CoAxiom Branched) Int [TcType] -- Int specifies branch number
                                                  -- See [CoAxiom Index] in Coercion.lhs
  | TcSymCo TcCoercion
  | TcTransCo TcCoercion TcCoercion
  | TcNthCo Int TcCoercion
  | TcLRCo LeftOrRight TcCoercion
  | TcCastCo TcCoercion TcCoercion     -- co1 |> co2
  | TcLetCo TcEvBinds TcCoercion
  deriving (Data.Data, Data.Typeable)

isEqVar :: Var -> Bool 
-- Is lifted coercion variable (only!)
isEqVar v = case tyConAppTyCon_maybe (varType v) of
               Just tc -> tc `hasKey` eqTyConKey
               Nothing -> False

isTcReflCo_maybe :: TcCoercion -> Maybe TcType
isTcReflCo_maybe (TcRefl ty) = Just ty
isTcReflCo_maybe _           = Nothing

isTcReflCo :: TcCoercion -> Bool
isTcReflCo (TcRefl {}) = True
isTcReflCo _           = False

getTcCoVar_maybe :: TcCoercion -> Maybe CoVar
getTcCoVar_maybe (TcCoVarCo v) = Just v
getTcCoVar_maybe _             = Nothing

mkTcReflCo :: TcType -> TcCoercion
mkTcReflCo = TcRefl

mkTcFunCo :: TcCoercion -> TcCoercion -> TcCoercion
mkTcFunCo co1 co2 = mkTcTyConAppCo funTyCon [co1, co2]

mkTcTyConAppCo :: TyCon -> [TcCoercion] -> TcCoercion
mkTcTyConAppCo tc cos   -- No need to expand type synonyms
                        -- See Note [TcCoercions]
  | Just tys <- traverse isTcReflCo_maybe cos 
  = TcRefl (mkTyConApp tc tys)    -- See Note [Refl invariant]

  | otherwise = TcTyConAppCo tc cos

mkTcAxInstCo :: CoAxiom br -> Int -> [TcType] -> TcCoercion
mkTcAxInstCo ax ind tys
  | arity == n_tys = TcAxiomInstCo ax_br ind tys
  | otherwise      = ASSERT( arity < n_tys )
                     foldl TcAppCo (TcAxiomInstCo ax_br ind (take arity tys))
                                   (map TcRefl (drop arity tys))
  where
    n_tys = length tys
    arity = coAxiomArity ax ind
    ax_br = toBranchedAxiom ax

mkTcUnbranchedAxInstCo :: CoAxiom Unbranched -> [TcType] -> TcCoercion
mkTcUnbranchedAxInstCo ax tys
  = mkTcAxInstCo ax 0 tys

mkTcAppCo :: TcCoercion -> TcCoercion -> TcCoercion
-- No need to deal with TyConApp on the left; see Note [TcCoercions]
mkTcAppCo (TcRefl ty1) (TcRefl ty2)     = TcRefl (mkAppTy ty1 ty2)
mkTcAppCo co1 co2                       = TcAppCo co1 co2

mkTcSymCo :: TcCoercion -> TcCoercion
mkTcSymCo co@(TcRefl {})  = co
mkTcSymCo    (TcSymCo co) = co
mkTcSymCo co              = TcSymCo co

mkTcTransCo :: TcCoercion -> TcCoercion -> TcCoercion
mkTcTransCo (TcRefl _) co = co
mkTcTransCo co (TcRefl _) = co
mkTcTransCo co1 co2       = TcTransCo co1 co2

mkTcNthCo :: Int -> TcCoercion -> TcCoercion
mkTcNthCo n (TcRefl ty) = TcRefl (tyConAppArgN n ty)
mkTcNthCo n co          = TcNthCo n co

mkTcLRCo :: LeftOrRight -> TcCoercion -> TcCoercion
mkTcLRCo lr (TcRefl ty) = TcRefl (pickLR lr (tcSplitAppTy ty))
mkTcLRCo lr co          = TcLRCo lr co

mkTcAppCos :: TcCoercion -> [TcCoercion] -> TcCoercion
mkTcAppCos co1 tys = foldl mkTcAppCo co1 tys

mkTcForAllCo :: Var -> TcCoercion -> TcCoercion
-- note that a TyVar should be used here, not a CoVar (nor a TcTyVar)
mkTcForAllCo tv (TcRefl ty) = ASSERT( isTyVar tv ) TcRefl (mkForAllTy tv ty)
mkTcForAllCo tv  co         = ASSERT( isTyVar tv ) TcForAllCo tv co

mkTcForAllCos :: [Var] -> TcCoercion -> TcCoercion
mkTcForAllCos tvs (TcRefl ty) = ASSERT( all isTyVar tvs ) TcRefl (mkForAllTys tvs ty)
mkTcForAllCos tvs co          = ASSERT( all isTyVar tvs ) foldr TcForAllCo co tvs

mkTcInstCos :: TcCoercion -> [TcType] -> TcCoercion
mkTcInstCos (TcRefl ty) tys = TcRefl (applyTys ty tys)
mkTcInstCos co tys          = foldl TcInstCo co tys

mkTcCoVarCo :: EqVar -> TcCoercion
-- ipv :: s ~ t  (the boxed equality type)
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
    go (TcRefl ty)            = Pair ty ty
    go (TcLetCo _ co)         = go co
    go (TcCastCo _ co)        = case getEqPredTys (pSnd (go co)) of
                                   (ty1,ty2) -> Pair ty1 ty2
    go (TcTyConAppCo tc cos)  = mkTyConApp tc <$> (sequenceA $ map go cos)
    go (TcAppCo co1 co2)      = mkAppTy <$> go co1 <*> go co2
    go (TcForAllCo tv co)     = mkForAllTy tv <$> go co
    go (TcInstCo co ty)       = go_inst co [ty]
    go (TcCoVarCo cv)         = eqVarKind cv
    go (TcAxiomInstCo ax ind tys)
      = let branch = coAxiomNthBranch ax ind
            tvs    = coAxBranchTyVars branch
        in Pair (substTyWith tvs tys (coAxNthLHS ax ind)) 
                (substTyWith tvs tys (coAxBranchRHS branch))
    go (TcSymCo co)           = swap (go co)
    go (TcTransCo co1 co2)    = Pair (pFst (go co1)) (pSnd (go co2))
    go (TcNthCo d co)         = tyConAppArgN d <$> go co
    go (TcLRCo lr co)         = (pickLR lr . tcSplitAppTy) <$> go co

    -- c.f. Coercion.coercionKind
    go_inst (TcInstCo co ty) tys = go_inst co (ty:tys)
    go_inst co               tys = (`applyTys` tys) <$> go co

eqVarKind :: EqVar -> Pair Type
eqVarKind cv
 | Just (tc, [_kind,ty1,ty2]) <- tcSplitTyConApp_maybe (varType cv)
 = ASSERT(tc `hasKey` eqTyConKey)
   Pair ty1 ty2
 | otherwise = pprPanic "eqVarKind, non coercion variable" (ppr cv <+> dcolon <+> ppr (varType cv))

coVarsOfTcCo :: TcCoercion -> VarSet
-- Only works on *zonked* coercions, because of TcLetCo
coVarsOfTcCo tc_co
  = go tc_co
  where
    go (TcRefl _)                = emptyVarSet
    go (TcTyConAppCo _ cos)      = foldr (unionVarSet . go) emptyVarSet cos
    go (TcAppCo co1 co2)         = go co1 `unionVarSet` go co2
    go (TcCastCo co1 co2)        = go co1 `unionVarSet` go co2
    go (TcForAllCo _ co)         = go co
    go (TcInstCo co _)           = go co
    go (TcCoVarCo v)             = unitVarSet v
    go (TcAxiomInstCo {})        = emptyVarSet
    go (TcSymCo co)              = go co
    go (TcTransCo co1 co2)       = go co1 `unionVarSet` go co2
    go (TcNthCo _ co)            = go co
    go (TcLRCo  _ co)            = go co
    go (TcLetCo (EvBinds bs) co) = foldrBag (unionVarSet . go_bind) (go co) bs
                                   `minusVarSet` get_bndrs bs
    go (TcLetCo {}) = emptyVarSet    -- Harumph. This does legitimately happen in the call
                                     -- to evVarsOfTerm in the DEBUG check of setEvBind

    -- We expect only coercion bindings, so use evTermCoercion 
    go_bind :: EvBind -> VarSet
    go_bind (EvBind _ tm) = go (evTermCoercion tm)

    get_bndrs :: Bag EvBind -> VarSet
    get_bndrs = foldrBag (\ (EvBind b _) bs -> extendVarSet bs b) emptyVarSet 

liftTcCoSubstWith :: [TyVar] -> [TcCoercion] -> TcType -> TcCoercion
-- This version can ignore capture; the free varialbes of the 
-- TcCoerion are all fresh.  Result is mush simpler code
liftTcCoSubstWith tvs cos ty
  = ASSERT( equalLength tvs cos )
    go ty
  where
    env = zipVarEnv tvs cos

    go ty@(TyVarTy tv)   = case lookupVarEnv env tv of
                             Just co -> co
                             Nothing -> mkTcReflCo ty
    go (AppTy t1 t2)     = mkTcAppCo (go t1) (go t2)
    go (TyConApp tc tys) = mkTcTyConAppCo tc (map go tys)
    go ty@(LitTy {})     = mkTcReflCo ty
    go (ForAllTy tv ty)  = mkTcForAllCo tv (go ty)
    go (FunTy t1 t2)     = mkTcFunCo (go t1) (go t2)
\end{code}

Pretty printing

\begin{code}
instance Outputable TcCoercion where
  ppr = pprTcCo

pprTcCo, pprParendTcCo :: TcCoercion -> SDoc
pprTcCo       co = ppr_co TopPrec   co
pprParendTcCo co = ppr_co TyConPrec co

ppr_co :: Prec -> TcCoercion -> SDoc
ppr_co _ (TcRefl ty) = angleBrackets (ppr ty)

ppr_co p co@(TcTyConAppCo tc [_,_])
  | tc `hasKey` funTyConKey = ppr_fun_co p co

ppr_co p (TcTyConAppCo tc cos)   = pprTcApp   p ppr_co tc cos
ppr_co p (TcLetCo bs co)         = maybeParen p TopPrec $
                                   sep [ptext (sLit "let") <+> braces (ppr bs), ppr co]
ppr_co p (TcAppCo co1 co2)       = maybeParen p TyConPrec $
                                   pprTcCo co1 <+> ppr_co TyConPrec co2
ppr_co p (TcCastCo co1 co2)      = maybeParen p FunPrec $
                                   ppr_co FunPrec co1 <+> ptext (sLit "|>") <+> ppr_co FunPrec co2
ppr_co p co@(TcForAllCo {})      = ppr_forall_co p co
ppr_co p (TcInstCo co ty)        = maybeParen p TyConPrec $
                                   pprParendTcCo co <> ptext (sLit "@") <> pprType ty
                     
ppr_co _ (TcCoVarCo cv)          = parenSymOcc (getOccName cv) (ppr cv)

ppr_co p (TcAxiomInstCo con ind cos)
  = pprPrefixApp p (ppr (getName con) <> brackets (ppr ind)) (map (ppr_type TyConPrec) cos)

ppr_co p (TcTransCo co1 co2) = maybeParen p FunPrec $
                               ppr_co FunPrec co1
                               <+> ptext (sLit ";")
                               <+> ppr_co FunPrec co2
ppr_co p (TcSymCo co)         = pprPrefixApp p (ptext (sLit "Sym")) [pprParendTcCo co]
ppr_co p (TcNthCo n co)       = pprPrefixApp p (ptext (sLit "Nth:") <+> int n) [pprParendTcCo co]
ppr_co p (TcLRCo lr co)       = pprPrefixApp p (ppr lr) [pprParendTcCo co]

ppr_fun_co :: Prec -> TcCoercion -> SDoc
ppr_fun_co p co = pprArrowChain p (split co)
  where
    split :: TcCoercion -> [SDoc]
    split (TcTyConAppCo f [arg,res])
      | f `hasKey` funTyConKey
      = ppr_co FunPrec arg : split res
    split co = [ppr_co TopPrec co]

ppr_forall_co :: Prec -> TcCoercion -> SDoc
ppr_forall_co p ty
  = maybeParen p FunPrec $
    sep [pprForAll tvs, ppr_co TopPrec rho]
  where
    (tvs,  rho) = split1 [] ty
    split1 tvs (TcForAllCo tv ty) = split1 (tv:tvs) ty
    split1 tvs ty                 = (reverse tvs, ty)
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

  | EvCast EvTerm TcCoercion     -- d |> co

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

  | EvLit EvLit                  -- Dictionary for class "SingI" for type lits.
                                 -- Note [SingI and EvLit]

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
    [G} (d |> ax7 a) :: a ~ Bool
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


Note [SingI and EvLit]
~~~~~~~~~~~~~~~~~~~~~~
A part of the type-level literals implementation is the class "SingI",
which provides a "smart" constructor for defining singleton values.
Here is the key stuff from GHC.TypeLits

  class SingI n where
    sing :: Sing n

  data family Sing (n::k)
  newtype instance Sing (n :: Nat)    = SNat Integer
  newtype instance Sing (s :: Symbol) = SSym String

Conceptually, this class has infinitely many instances:

  instance Sing 0       where sing = SNat 0
  instance Sing 1       where sing = SNat 1
  instance Sing 2       where sing = SNat 2
  instance Sing "hello" where sing = SSym "hello"
  ...

In practice, we solve "SingI" predicates in the type-checker because we can't
have infinately many instances.  The evidence (aka "dictionary")
for "SingI (n :: Nat)" is of the form "EvLit (EvNum n)".

We make the following assumptions about dictionaries in GHC:
  1. The "dictionary" for classes with a single method---like SingI---is
     a newtype for the type of the method, so using a evidence amounts
     to a coercion, and
  2. Newtypes use the same representation as their definition types.

So, the evidence for "SingI" is just a value of the representation type,
wrapped in two newtype constructors: one to make it into a "Sing" value,
and another to make it into "SingI" evidence.


\begin{code}
mkEvCast :: EvTerm -> TcCoercion -> EvTerm
mkEvCast ev lco
  | isTcReflCo lco = ev
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


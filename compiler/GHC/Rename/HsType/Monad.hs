
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Rename.HsType.Monad
  ( -- Monad
    RnTypeM, runRnTypeM, runRnTypeMNoBndrs,
    RnTypeBinders(..), liftRnFV, liftTyRnCps, liftRnM, mapRnMScoped, xoptTyM,
    FreeVarsPatcher, BinderType(..), ActionOnBinder(..),
    -- assertions
    assertNoTypeBndrs, assertNoImpBndrs, assertNoExpBndrs, assertNoNWCsBndrs,
    -- src spans
    setSrcSpanTypeM, setSrcSpanATypeM,
    -- writer api
    tellTypeBinders, explicitTypeBinder, implicitTypeBinder, tellNamedWildCard,
    -- reader api
    askFreeVarsPatcher, askDocContext, askLevel, askLocals, askNamedWildCards, whenOnKind,
    addNamesToLocals, askNamedWildCardMode, askBindMode, askWhatWeRename,
    collectNamedWildcards, atLevel, atKindLevel, renameInContext, renameTypeAs,
    dontBind, bindExplicitly, bindImplicitly, localBindMode, localActionOnBinder,
    -- lookups
    lookupTypeOccRnTyM, onLookupFailRnTyM, lookupTypeOccRnTyM_maybe,
    -- configuration types
    RnTyKiWhat(..), BindMode(..),
    -- helpers
    bindLocalNamesFVType,
  ) where

import GHC.Prelude


import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr ( pprHsDocContext )
import GHC.Tc.Utils.Monad
import GHC.Types.Name
import GHC.Types.Name.Set

import GHC.Types.Basic  ( TypeOrKind(..) )
import GHC.Utils.Outputable
import qualified GHC.LanguageExtensions as LangExt


import Control.Monad.Trans.Reader
import GHC.Rename.Cps
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS
import GHC.Data.Bag
import qualified Data.Semigroup as S
import GHC.Parser.Annotation (SrcSpanAnn' (locA), LocatedN)
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader (RdrName, greName)
import GHC.Rename.Env (lookupTypeOccRn, lookupOccRn_maybe, onLookupFailRn)
import Control.Monad.Trans.State
import GHC.Stack (HasCallStack)
import GHC.Utils.Panic.Plain (massert)
import GHC.Rename.Utils (bindLocalNamesFV, warnUnusedMatches)
import Data.Function ((&))


-- | Type renaming monad
-- For the OccSet in the ReaderT, see Note [Locally bound names in type patterns]
-- For the HsTyPatRnBuilderRn in the WriterT, see Note [Implicit and explicit type variable binders]
-- For the CpsRn base monad, see Note [CpsRn monad]
-- For why we need CpsRn in TPRnM see Note [Left-to-right scoping of type patterns]
newtype RnTypeM a =
  RnTyM {
    run_rn_type_m :: ReaderT ActionOnBinder
                   ( ReaderT OccSet -- forall-bound names
                   ( ReaderT RnTyKiEnv -- general rename environment
                   ( StateT (Bag Name) -- wildcards
                   ( WriterT RnTypeBindersBag -- explicit and implicit binders
                     CpsRn -- Cps-based rename monad, see Note [Left-to-right scoping of type patterns]
                     )))) a
  } deriving newtype (Functor, Applicative, Monad)


type FreeVarsPatcher = LocatedN Name -> FreeVars -> RnM FreeVars

composePatchers :: FreeVarsPatcher -> FreeVarsPatcher -> FreeVarsPatcher
composePatchers patcher1 patcher2 name fvs = do
  fvs' <- patcher1 name fvs
  patcher2 name fvs'

defFreeVarsPatcher :: FreeVarsPatcher
defFreeVarsPatcher name fvs = pure (delFV (unLoc name) fvs)

data ActionOnBinder =
  MkAOB {
    exp_bndr_action :: FreeVarsPatcher,
    imp_bndr_action :: FreeVarsPatcher,
    nwc_bndr_action :: FreeVarsPatcher
  }

defActionOnBinder :: ActionOnBinder
defActionOnBinder = MkAOB {
    exp_bndr_action = warn_unused_exp_bndr `composePatchers` defFreeVarsPatcher,
    imp_bndr_action = defFreeVarsPatcher,
    nwc_bndr_action = defFreeVarsPatcher
  }
  where
    warn_unused_exp_bndr name fvs = do
      warnUnusedMatches [unLoc name] fvs
      pure fvs

data BinderType = ExplicitBinder | ImplicitBinder | NamedWildCardBinder


askFreeVarsPatcher :: BinderType -> RnTypeM FreeVarsPatcher
askFreeVarsPatcher bndr_type = do
  action_on_bndr <- ask_act_on_bndr
  case bndr_type of
    ExplicitBinder -> pure (exp_bndr_action action_on_bndr)
    ImplicitBinder -> pure (imp_bndr_action action_on_bndr)
    NamedWildCardBinder -> pure (nwc_bndr_action action_on_bndr)
  where
    ask_act_on_bndr = RnTyM ask

localActionOnBinder :: (ActionOnBinder -> ActionOnBinder) -> RnTypeM a -> RnTypeM a
localActionOnBinder f = RnTyM . local f . run_rn_type_m

data RnTypeBindersBag =
  MkRnTyBndrsBag {
    imp_bndrs_bag :: Bag Name,
    exp_bndrs_bag :: Bag Name
  }

instance Semigroup RnTypeBindersBag where
  MkRnTyBndrsBag imp_tvs1 exptvs1 <> MkRnTyBndrsBag imp_tvs2 exptvs2 =
    MkRnTyBndrsBag
      (imp_tvs1 `unionBags` imp_tvs2)
      (exptvs1 `unionBags` exptvs2)

instance Monoid RnTypeBindersBag where
  mempty = MkRnTyBndrsBag emptyBag emptyBag

data RnTypeBinders =
  MkRnTyBndrs {
    imp_bndrs :: [Name],
    exp_bndrs :: [Name],
    nwc_bndrs :: [Name]
  }

runRnTypeM :: HsDocContext -> RnTypeM a ->  RnM (a, RnTypeBinders, FreeVars)
runRnTypeM doc (RnTyM action) = do
  (((res, nwcs_bag), bndrs_bag), fvs) <-
    action `runReaderT` defActionOnBinder
           `runReaderT` emptyOccSet
           `runReaderT` mkTyKiEnv doc
           `runStateT` emptyBag
          & runWriterT
          & runCps

  let ty_bndrs = build_ty_bndrs nwcs_bag bndrs_bag
  traceRn "runRnTypeM binders" $ vcat
    [ text "imp_bndrs" <+> ppr (imp_bndrs ty_bndrs)
    , text "exp_bndrs" <+> ppr (exp_bndrs ty_bndrs)
    , text "nwc_bndrs" <+> ppr (nwc_bndrs ty_bndrs)
    ]
  pure (res, ty_bndrs, fvs)
  where
    build_ty_bndrs nwcs MkRnTyBndrsBag{imp_bndrs_bag = imp, exp_bndrs_bag = exp} =
      MkRnTyBndrs
        { imp_bndrs = bagToList imp
        , exp_bndrs = bagToList exp
        , nwc_bndrs = bagToList nwcs
        }

runRnTypeMNoBndrs :: HasCallStack => HsDocContext -> RnTypeM a ->  RnM (a, FreeVars)
runRnTypeMNoBndrs doc action = do
  (res, ty_bndrs, fvs) <- runRnTypeM doc action
  assertNoTypeBndrs ty_bndrs
  pure (res, fvs)

assertNoTypeBndrs :: HasCallStack => RnTypeBinders -> RnM ()
assertNoTypeBndrs ty_bndrs = do
  assertNoImpBndrs (imp_bndrs ty_bndrs)
  assertNoExpBndrs (exp_bndrs ty_bndrs)
  assertNoNWCsBndrs (nwc_bndrs ty_bndrs)

assertNoImpBndrs :: HasCallStack => [Name] -> RnM ()
assertNoImpBndrs = massert . null

assertNoExpBndrs :: HasCallStack => [Name] -> RnM ()
assertNoExpBndrs = massert . null

assertNoNWCsBndrs :: HasCallStack => [Name] -> RnM ()
assertNoNWCsBndrs = massert . null

tellTypeBinders :: RnTypeBindersBag -> RnTypeM ()
tellTypeBinders = RnTyM . lift . lift . lift . lift . tell

explicitTypeBinder :: Name -> RnTypeBindersBag
explicitTypeBinder name = mempty {exp_bndrs_bag = unitBag name}

implicitTypeBinder :: Name -> RnTypeBindersBag
implicitTypeBinder name = mempty {imp_bndrs_bag = unitBag name}

tellNamedWildCard :: Name -> RnTypeM ()
tellNamedWildCard name = RnTyM $ lift $ lift $ lift $ modify (`unionBags` unitBag name)


mapRnMScoped :: (forall a . RnM (a, FreeVars) -> RnM (a, FreeVars)) -- perform action
             -> (forall a . RnM (a, FreeVars) -> RnM (a, FreeVars)) -- revert everything back for continuation
             -> RnTypeM a
             -> RnTypeM a
mapRnMScoped action reverter = hoistCpsRn $ \(CpsRn thing_inside) ->
  liftCpsWithCont $ \continuation ->
    action $ thing_inside $ \thing_inside_res ->
      reverter (continuation thing_inside_res)

setSrcSpanATypeM :: SrcSpanAnn' ann -> RnTypeM a -> RnTypeM a
setSrcSpanATypeM l = setSrcSpanTypeM (locA l)

setSrcSpanTypeM :: SrcSpan -> RnTypeM a -> RnTypeM a
setSrcSpanTypeM new_loc action = do
  old_loc <- liftRnM getSrcSpanM
  mapRnMScoped (setSrcSpan new_loc) (setSrcSpan old_loc) action

addNamesToLocals :: [Name] -> RnTypeM a -> RnTypeM a
addNamesToLocals names (RnTyM thing_inside) = RnTyM $ ReaderT $ \action_on_bndr ->
    local (`extendOccSetList` map occName names) (thing_inside `runReaderT` action_on_bndr)

hoistCpsRn ::  (forall a . CpsRn a -> CpsRn a) -> RnTypeM a -> RnTypeM a
hoistCpsRn mapper thing_inside =
  wrap_monad_stack $ \action_on_bndr occ_set rtke nwcs ->
    mapper $
  unwrap_monad_stack thing_inside action_on_bndr occ_set rtke nwcs
  where
    unwrap_monad_stack thing_inside action_on_bndr occ_set rtke nwcs =
      run_rn_type_m thing_inside
         `runReaderT` action_on_bndr
         `runReaderT` occ_set
         `runReaderT` rtke
         `runStateT` nwcs
        & runWriterT

    wrap_monad_stack thing_inside =
      RnTyM $
      ReaderT $ \action_on_bndr ->
      ReaderT $ \occ_set ->
      ReaderT $ \rtke ->
      StateT $ \nwcs ->
      writerT $ thing_inside action_on_bndr occ_set rtke nwcs

liftRnM :: RnM a -> RnTypeM a
liftRnM = liftTyRnCps . liftCps

liftRnFV :: RnM (a, FreeVars) -> RnTypeM a
liftRnFV = liftTyRnCps . liftCpsFV

liftTyRnCps :: CpsRn a -> RnTypeM a
liftTyRnCps = RnTyM . lift . lift . lift . lift . lift

whenOnKind :: RnTypeM () -> RnTypeM ()
whenOnKind thing_inside = do
  level <- askLevel
  case level of
    KindLevel -> thing_inside
    _ -> pure ()

askNamedWildCards :: RnTypeM NameSet
askNamedWildCards = do
  nwcs_bag <- get_nwcs
  pure (mkNameSet (bagToList nwcs_bag))
  where
    get_nwcs = RnTyM $ lift $ lift $ lift $ get

askLocals :: RnTypeM OccSet
askLocals = RnTyM $ lift ask

askRnTyKiEnv :: RnTypeM RnTyKiEnv
askRnTyKiEnv = RnTyM $ lift $ lift ask

askDocContext :: RnTypeM HsDocContext
askDocContext = rtke_ctxt <$> askRnTyKiEnv

askLevel :: RnTypeM TypeOrKind
askLevel = rtke_level <$> askRnTyKiEnv

askWhatWeRename :: RnTypeM RnTyKiWhat
askWhatWeRename = rtke_what <$> askRnTyKiEnv

askBindMode :: RnTypeM BindMode
askBindMode = rtke_bind_mode <$> askRnTyKiEnv

askNamedWildCardMode :: RnTypeM Bool
askNamedWildCardMode = rtke_nwcs <$> askRnTyKiEnv

lookupTypeOccRnTyM :: RdrName -> RnTypeM Name
lookupTypeOccRnTyM rdr_name = liftRnFV $ do
  name <- lookupTypeOccRn rdr_name
  pure (name, unitFV name)

onLookupFailRnTyM :: RdrName -> RnTypeM Name
onLookupFailRnTyM rdr_name = liftRnFV $ do
  name <- onLookupFailRn rdr_name
  pure (name, unitFV name)

lookupTypeOccRnTyM_maybe :: RdrName -> RnTypeM (Maybe Name)
lookupTypeOccRnTyM_maybe rdr_name = liftRnFV $ do
  m_gre <- lookupOccRn_maybe rdr_name
  case m_gre of
    Just gre -> let name = greName gre in pure (Just name, unitFV name)
    Nothing -> pure (Nothing, emptyFVs)


bindLocalNamesFVType :: [Name] -> RnM (a, RnTypeBinders, FreeVars) -> RnM (a, RnTypeBinders, FreeVars)
bindLocalNamesFVType names thing_inside =
  fromPair <$> bindLocalNamesFV names (toPair <$> thing_inside)
  where
    toPair (a,b,c) = ((a,b), c)
    fromPair ((a,b), c) = (a,b,c)


localRnTyKiEnv :: (RnTyKiEnv -> RnTyKiEnv) -> RnTypeM a -> RnTypeM a
localRnTyKiEnv action (RnTyM thing_inside) = RnTyM $ ReaderT $ \action_on_bndr -> ReaderT $ \occ_set ->
  local action (thing_inside `runReaderT` action_on_bndr `runReaderT`  occ_set)

localBindMode :: (BindMode -> BindMode) -> RnTypeM a -> RnTypeM a
localBindMode f = localRnTyKiEnv $ \env -> env {rtke_bind_mode = f (rtke_bind_mode env)}

bindImplicitly :: RnTypeM a -> RnTypeM a
bindImplicitly = localBindMode (const BindImplicitly)

bindExplicitly :: RnTypeM a -> RnTypeM a
bindExplicitly = localBindMode (const BindExplicitly)

dontBind :: RnTypeM a -> RnTypeM a
dontBind = localBindMode (const DontBind)

renameInContext :: HsDocContext -> RnTypeM a -> RnTypeM a
renameInContext ctxt = localRnTyKiEnv $ \env -> env {rtke_ctxt = ctxt}

renameTypeAs :: RnTyKiWhat -> RnTypeM a -> RnTypeM a
renameTypeAs what = localRnTyKiEnv $ \env -> env {rtke_what = what}

atKindLevel :: RnTypeM a -> RnTypeM a
atKindLevel = atLevel KindLevel

atLevel :: TypeOrKind -> RnTypeM a -> RnTypeM a
atLevel level = localRnTyKiEnv $ \env -> env {rtke_level = level}

collectNamedWildcards :: RnTypeM a -> RnTypeM a
collectNamedWildcards = localRnTyKiEnv $ \env -> env {rtke_nwcs = True}

xoptTyM :: LangExt.Extension -> RnTypeM Bool
xoptTyM = liftRnM . xoptM

data RnTyKiEnv
  = RTKE { rtke_ctxt      :: HsDocContext
         , rtke_level     :: TypeOrKind  -- Am I renaming a type or a kind?
         , rtke_what      :: RnTyKiWhat  -- And within that what am I renaming?
         , rtke_nwcs      :: Bool        -- Should RnTypeM collect named wildcards or no
         , rtke_bind_mode :: BindMode
    }

data BindMode = BindExplicitly | BindImplicitly | DontBind

data RnTyKiWhat = RnTypeBody
                | RnTopConstraint   -- Top-level context of HsSigWcTypes
                | RnConstraint      -- All other constraints

instance Outputable RnTyKiEnv where
  ppr (RTKE { rtke_level = lev, rtke_what = what
            , rtke_nwcs = wcs, rtke_ctxt = ctxt })
    = text "RTKE"
      <+> braces (sep [ ppr lev, ppr what, ppr wcs
                      , pprHsDocContext ctxt ])

instance Outputable RnTyKiWhat where
  ppr RnTypeBody      = text "RnTypeBody"
  ppr RnTopConstraint = text "RnTopConstraint"
  ppr RnConstraint    = text "RnConstraint"

mkTyKiEnv :: HsDocContext -> RnTyKiEnv
mkTyKiEnv cxt
 = RTKE { rtke_level = TypeLevel, rtke_nwcs = False
        , rtke_what = RnTypeBody, rtke_ctxt = cxt
        , rtke_bind_mode = DontBind }

-- TODO sand-witch: rewrite
{- Note [Locally bound names in type patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type patterns can bind local names using forall. Compare the following examples:
  f (Proxy @(Either a b)) = ...
  g (Proxy @(forall a . Either a b)) = ...

In `f` both `a` and `b` are bound by the pattern and scope over the RHS of f.
In `g` only `b` is bound by the pattern, whereas `a` is locally bound in the pattern
and does not scope over the RHS of `g`.

We track locally bound names in the `OccSet` in `TPRnM` monad, and use it to
decide whether occurences of type variables are usages or bindings.

The check is done in `rn_ty_pat_var`

Note [Implicit and explicit type variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type patterns are renamed differently from ordinary types.
  * Types are renamed by `rnHsType` where all type variable occurences are considered usages
  * Type patterns are renamed by `rnHsTyPat` where some type variable occurences are usages
    and other are bindings

Here is an example:
  {-# LANGUAGE ScopedTypeVariables #-}
  f :: forall b. Proxy _ -> ...
  f (Proxy @(x :: (a, b))) = ...

In the (x :: (a,b)) type pattern
  * `x` is a type variable explicitly bound by type pattern
  * `a` is a type variable implicitly bound in a pattern signature
  * `b` is a usage of type variable bound by the outer forall

This classification is clear to us in `rnHsTyPat`, but it is also useful in later passes, such
as `collectPatBinders` and `tcHsTyPat`, so we store it in the extension field of `HsTyPat`, namely
`HsTyPatRn`.

To collect lists of those variables efficiently we use `HsTyPatRnBuilder` which is exactly like
`HsTyPatRn`, but uses Bags.

Note [Left-to-right scoping of type patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In term-level patterns, we use continuation passing to implement left-to-right
scoping, see Note [CpsRn monad]. Left-to-right scoping manifests itself when
e.g. view patterns are involved:

  f (x, g x -> Just y) = ...

Here the first occurrence of `x` is a binder, and the second occurrence is a
use of `x` in a view pattern. This example does not work if we swap the
components of the tuple:

  f (g x -> Just y, x) = ...
  --  ^^^
  -- Variable not in scope: x

In type patterns there are no view patterns, but there is a different feature
that is served well by left-to-right scoping: kind annotations. Compare:

  f (Proxy @(T k (a :: k))) = ...
  g (Proxy @(T (a :: k) k)) = ...

In `f`, the first occurrence of `k` is an explicit binder,
  and the second occurrence is a usage. Simple.
In `g`, the first occurrence of `k` is an implicit binder,
  and then the second occurrence is an explicit binder that shadows it.

So we get two different results after renaming:

  f (Proxy @(T k1 (a :: k1))) = ...
  g (Proxy @(T (a :: k1) k2)) = ...

This makes GHC accept the first example but rejects the second example with an
error about duplicate binders.

One could argue that we don't want order-sensitivity here. Historically, we
used a different principle when renaming types: collect all free variables,
bind them on the outside, and then rename all occurrences as usages.
This approach does not scale to multiple patterns. Consider:

  f' (MkP @k @(a :: k)) = ...
  g' (MkP @(a :: k) @k) = ...

Here a difference in behavior is inevitable, as we rename type patterns
one at a time. Could we perhaps concatenate the free variables from all
type patterns in a ConPat? But then we still get the same problem one level up,
when we have multiple patterns in a function LHS

  f'' (Proxy @k) (Proxy @(a :: k)) = ...
  g'' (Proxy @(a :: k)) (Proxy @k) = ...

And if we tried to avoid order sensitivity at this level, then we'd still be left
with lambdas:

  f''' (Proxy @k)        = \(Proxy @(a :: k)) -> ...
  g''' (Proxy @(a :: k)) = \(Proxy @k)        -> ...


So we have at least three options where we could do free variable extraction:
HsConPatTyArg, ConPat, or a Match (used to represent a function LHS). And none
of those would be general enough. Rather than make an arbitrary choice, we
embrace left-to-right scoping in types and implement it with CPS, just like
it's done for view patterns in terms.
-}

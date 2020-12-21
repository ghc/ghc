{-# LANGUAGE CPP #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


************************************************************************

               Static Argument Transformation pass

************************************************************************

May be seen as removing invariants from loops:
Arguments of recursive functions that do not change in recursive
calls are removed from the recursion, which is done locally
and only passes the arguments which effectively change.

Example:
map = /\ ab -> \f -> \xs -> case xs of
                 []       -> []
                 (a:b) -> f a : map f b

as map is recursively called with the same argument f (unmodified)
we transform it to

map = /\ ab -> \f -> \xs -> let map' ys = case ys of
                       []     -> []
                       (a:b) -> f a : map' b
                in map' xs

Notice that for a compiler that uses lambda lifting this is
useless as map' will be transformed back to what map was.

We could possibly do the same for big lambdas, but we don't as
they will eventually be removed in later stages of the compiler,
therefore there is no penalty in keeping them.

We only apply the SAT when the number of static args is > 2. This
produces few bad cases.  See
                should_transform
in saTransform.

Here are the headline nofib results:
                  Size    Allocs   Runtime
Min             +0.0%    -13.7%    -21.4%
Max             +0.1%     +0.0%     +5.4%
Geometric Mean  +0.0%     -0.2%     -6.9%

The previous patch, to fix polymorphic floatout demand signatures, is
essential to make this work well!
-}

module GHC.Core.Opt.StaticArgs ( satAnalProgram, doStaticArgs, saTransform ) where

import GHC.Prelude

#include "HsVersions.h"

import GHC.Builtin.Names ( unboundKey )
import GHC.Types.Var
import GHC.Core
import GHC.Core.Utils
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.Unfold
import GHC.Core.Unfold.Make
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Var.Env
import GHC.Types.Unique.Supply
import GHC.Utils.Misc
import GHC.Types.Basic
import GHC.Types.Unique.FM
import GHC.Types.Var.Set
import GHC.Types.Unique.Set
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.Maybe

import Data.List (mapAccumL)
import Data.Bifunctor (second)

-- import GHC.Driver.Ppr

satAnalProgram :: CoreProgram -> CoreProgram
satAnalProgram bs = map (snd . satAnalBind initSatEnv TopLevel) bs

-- | Lambda binders ('TyVar's, 'CoVar's and 'Id's) of a let-bound RHS, thus
-- parameters to a function.
type Params = [Var]

data SatEnv
  = SE
  { se_params_env :: !(IdEnv Params)
  -- ^ Lambda binders of interesting Id's. If a param is static, then all
  -- occurrences must have the 'Var' listed here in its position!
  , se_in_scope   :: !InScopeSet
  -- ^ Needed for handling shadowing properly. See 'addInScopeVars'.
  }

initSatEnv :: SatEnv
initSatEnv = SE emptyVarEnv emptyInScopeSet

addInterestingId :: SatEnv -> Id -> Params -> SatEnv
addInterestingId env id params =
  env { se_params_env = extendVarEnv (se_params_env env) id params }

lookupInterestingId :: SatEnv -> Id -> Maybe Params
lookupInterestingId env id = lookupVarEnv (se_params_env env) id

addInScopeVar :: SatEnv -> Var -> SatEnv
addInScopeVar env v = addInScopeVars env [v]

addInScopeVars :: SatEnv -> [Var] -> SatEnv
addInScopeVars se vars = se { se_in_scope = in_scope', se_params_env = env' }
  where
    in_scope  = se_in_scope se
    in_scope' = extendInScopeSetList in_scope vars
    env       = se_params_env se
    var_set   = mkVarSet vars
    env'
      | any (`elemInScopeSet` in_scope) vars
      = mapVarEnv (hideShadowedParams var_set) $ delVarEnvList env vars
      | otherwise
      = env

hideShadowedParams :: VarSet -> Params -> Params
hideShadowedParams shadowing_vars = map_if shadowed hide_param
  where
    map_if :: (a -> Bool) -> (a -> a) -> [a] -> [a]
    map_if p f       = map (\a -> if p a then f a else a)
    shadowed param   = param `elemVarSet` shadowing_vars
    -- unboundKey is guaranteed not to occur anywhere in the program!
    -- See Note [Shadowed Params] TODO
    hide_param param = param `setVarUnique` unboundKey

newtype SatOccs = SO (IdEnv StaticArgs)

emptySatOccs :: SatOccs
emptySatOccs = SO emptyVarEnv

addSatOccs :: SatOccs -> Id -> StaticArgs -> SatOccs
addSatOccs (SO env) fn static_args =
  SO $ extendVarEnv_C andStaticArgs env fn static_args

combineSatOccs :: SatOccs -> SatOccs -> SatOccs
combineSatOccs (SO a) (SO b) = SO $ plusVarEnv_C andStaticArgs a b

combineSatOccsList :: [SatOccs] -> SatOccs
combineSatOccsList occs = foldl' combineSatOccs emptySatOccs occs

peelSatOccs :: SatOccs -> Id -> (StaticArgs, SatOccs)
peelSatOccs (SO env) fn = case delLookupVarEnv env fn of
  (mb_sa, env') -> (mb_sa `orElse` noStaticArgs, SO env')

satAnalBind :: SatEnv -> TopLevelFlag -> CoreBind -> (SatOccs, CoreBind)
satAnalBind env top_lvl (NonRec id rhs) = (occs, NonRec id rhs')
  where
    (occs, rhs') = satAnalExpr (env `addInScopeVar` id) rhs
satAnalBind env top_lvl (Rec pairs) = (combineSatOccsList occss, Rec pairs')
  where
    (occss, pairs') = mapAndUnzip anal_one pairs
    anal_one (fn, rhs) = (occs', (fn', rhs'))
      where
        (bndrs, rhs_body)    = collectBinders rhs
        env1                 = env `addInScopeVars` (fn:bndrs)
        env2 | notNull bndrs = addInterestingId env1 fn bndrs
             | otherwise     = env1
        (occs, rhs_body')    = satAnalExpr env2 rhs_body
        rhs'                 = mkLams bndrs rhs_body'
        (static_args, occs') = peelSatOccs occs fn
        !fn' | allStaticAndUnliftedBody (length bndrs) static_args rhs_body
             || (isStableUnfolding (realIdUnfolding fn) && idStaticArgs fn /= noStaticArgs)
             = fn -- otherwise we end up with an unlifted worker body
             | otherwise     = -- pprTrace "satAnalBind:set" (ppr fn $$ ppr static_args) $
                               fn `setIdStaticArgs` static_args
                                  `setIdUnfolding` sat_unf
        sat_unf
          | Just unf <- mkSatUnfolding top_lvl fn rhs = unf
          | otherwise = realIdUnfolding fn


allStaticAndUnliftedBody :: Arity -> StaticArgs -> CoreExpr -> Bool
allStaticAndUnliftedBody arty sa body =
  count is_static (getStaticArgs sa) == arty
  && isUnliftedType (exprType body)
  where
    is_static Static{}    = True
    is_static NotStatic{} = False

satAnalExpr :: SatEnv -> CoreExpr -> (SatOccs, CoreExpr)
satAnalExpr _   e@(Lit _)      = (emptySatOccs, e)
satAnalExpr _   e@(Coercion _) = (emptySatOccs, e)
satAnalExpr _   e@(Type _)     = (emptySatOccs, e)
satAnalExpr _   e@(Var _)      = (emptySatOccs, e) -- boring! See the App case
satAnalExpr env (Tick t e)     = second (Tick t)      $ satAnalExpr env e
satAnalExpr env (Cast e c)     = second (flip Cast c) $ satAnalExpr env e
satAnalExpr env e@App{}        = uncurry (satAnalApp env) (collectArgs e)
satAnalExpr env e@Lam{}        = (occs, mkLams bndrs body')
  where
    (bndrs, body) = collectBinders e
    (occs, body') = satAnalExpr (env `addInScopeVars` bndrs) body
satAnalExpr env (Let bnd body) = (occs, Let bnd' body')
  where
    (occs_bind, bnd')  = satAnalBind env NotTopLevel bnd
    (occs_body, body') = satAnalExpr (env `addInScopeVars` bindersOf bnd) body
    !occs              = combineSatOccs occs_body occs_bind
satAnalExpr env (Case scrut bndr ty alts) = (occs, Case scrut' bndr ty alts')
  where
    (occs_scrut, scrut') = satAnalExpr env scrut
    alt_env              = env `addInScopeVar` bndr
    (occs_alts,  alts')  = mapAndUnzip (satAnalAlt alt_env) alts
    occs                 = combineSatOccsList (occs_scrut:occs_alts)

satAnalAlt :: SatEnv -> CoreAlt -> (SatOccs, CoreAlt)
satAnalAlt env (dc, bndrs, rhs) = (occs, (dc, bndrs, rhs'))
  where
    (occs, rhs') = satAnalExpr (env `addInScopeVars` bndrs) rhs

satAnalApp :: SatEnv -> CoreExpr -> [CoreArg] -> (SatOccs, CoreExpr)
satAnalApp env head args = (add_static_args_info occs, mkApps head' args')
  where
    (occs_head, head') = satAnalExpr env head
    (occs_args, args') = mapAndUnzip (satAnalExpr env) args
    occs               = combineSatOccsList (occs_head:occs_args)
    add_static_args_info occs
      | Var fn <- head, Just params <- lookupInterestingId env fn
      = addSatOccs occs fn (mkStaticArgs $ zipWith asStaticArg params args)
      | otherwise
      = occs

asStaticArg :: Var -> CoreArg -> Staticness ()
asStaticArg v arg
  | isId v,         Var id <- arg, v == id                     = Static ()
  | isTyVar v,      Type t <- arg, mkTyVarTy v `eqType` t      = Static ()
  | isCoVar v, Coercion co <- arg, mkCoVarCo v `eqCoercion` co = Static ()
  | otherwise                                                  = NotStatic

doStaticArgs :: UniqSupply -> CoreProgram -> CoreProgram
doStaticArgs us binds = snd $ mapAccumL sat_bind_threaded_us us binds
  where
    sat_bind_threaded_us us bind =
        let (us1, us2) = splitUniqSupply us
        in (us1, fst $ runSAT us2 (satBind bind emptyUniqSet))

-- We don't bother to SAT recursive groups since it can lead
-- to massive code expansion: see Andre Santos' thesis for details.
-- This means we only apply the actual SAT to Rec groups of one element,
-- but we want to recurse into the others anyway to discover other binds
satBind :: CoreBind -> IdSet -> SatM (CoreBind, IdSATInfo)
satBind (NonRec binder rhs) interesting_ids = do
    (rhs', sat_info_rhs) <- satTopLevelExpr rhs interesting_ids
    return (NonRec binder rhs', sat_info_rhs)
satBind (Rec [(binder, rhs)]) interesting_ids = do
    let interesting_ids' = interesting_ids `addOneToUniqSet` binder
        (rhs_binders, rhs_body) = collectBinders rhs
    (rhs_body', sat_info_rhs_body) <- satTopLevelExpr rhs_body interesting_ids'
    let sat_info_rhs_from_args = unitVarEnv binder (bindersToSATInfo rhs_binders)
        sat_info_rhs' = mergeIdSATInfo sat_info_rhs_from_args sat_info_rhs_body

        shadowing = binder `elementOfUniqSet` interesting_ids
        sat_info_rhs'' = if shadowing
                        then sat_info_rhs' `delFromUFM` binder -- For safety
                        else sat_info_rhs'

    bind' <- saTransformMaybe binder (lookupUFM sat_info_rhs' binder)
                              rhs_binders rhs_body'
    return (bind', sat_info_rhs'')
satBind (Rec pairs) interesting_ids = do
    let (binders, rhss) = unzip pairs
    rhss_SATed <- mapM (\e -> satTopLevelExpr e interesting_ids) rhss
    let (rhss', sat_info_rhss') = unzip rhss_SATed
    return (Rec (zipEqual "satBind" binders rhss'), mergeIdSATInfos sat_info_rhss')

data App = VarApp Id | TypeApp Type | CoApp Coercion

type IdAppInfo = (Id, SATInfo)

type SATInfo = [Staticness App]
type IdSATInfo = IdEnv SATInfo
emptyIdSATInfo :: IdSATInfo
emptyIdSATInfo = emptyUFM

{-
pprIdSATInfo id_sat_info = vcat (map pprIdAndSATInfo (Map.toList id_sat_info))
  where pprIdAndSATInfo (v, sat_info) = hang (ppr v <> colon) 4 (pprSATInfo sat_info)
-}

pprSATInfo :: SATInfo -> SDoc
pprSATInfo staticness = hcat $ map pprStaticness staticness

pprStaticness :: Staticness App -> SDoc
pprStaticness (Static (VarApp _))  = text "SV"
pprStaticness (Static (TypeApp _)) = text "ST"
pprStaticness (Static (CoApp _))   = text "SC"
pprStaticness NotStatic            = text "NS"


mergeSATInfo :: SATInfo -> SATInfo -> SATInfo
mergeSATInfo l r = zipWith mergeSA l r
  where
    mergeSA NotStatic _ = NotStatic
    mergeSA _ NotStatic = NotStatic
    mergeSA (Static (VarApp v)) (Static (VarApp v'))
      | v == v'   = Static (VarApp v)
      | otherwise = NotStatic
    mergeSA (Static (TypeApp t)) (Static (TypeApp t'))
      | t `eqType` t' = Static (TypeApp t)
      | otherwise     = NotStatic
    mergeSA (Static (CoApp c)) (Static (CoApp c'))
      | c `eqCoercion` c' = Static (CoApp c)
      | otherwise             = NotStatic
    mergeSA _ _  = pprPanic "mergeSATInfo" $
                          text "Left:"
                       <> pprSATInfo l <> text ", "
                       <> text "Right:"
                       <> pprSATInfo r

mergeIdSATInfo :: IdSATInfo -> IdSATInfo -> IdSATInfo
mergeIdSATInfo = plusUFM_C mergeSATInfo

mergeIdSATInfos :: [IdSATInfo] -> IdSATInfo
mergeIdSATInfos = foldl' mergeIdSATInfo emptyIdSATInfo

bindersToSATInfo :: [Id] -> SATInfo
bindersToSATInfo vs = map (Static . binderToApp) vs
    where binderToApp v | isId v    = VarApp v
                        | isTyVar v = TypeApp $ mkTyVarTy v
                        | otherwise = CoApp $ mkCoVarCo v

finalizeApp :: Maybe IdAppInfo -> IdSATInfo -> IdSATInfo
finalizeApp Nothing id_sat_info = id_sat_info
finalizeApp (Just (v, sat_info')) id_sat_info =
    let sat_info'' = case lookupUFM id_sat_info v of
                        Nothing -> sat_info'
                        Just sat_info -> mergeSATInfo sat_info sat_info'
    in extendVarEnv id_sat_info v sat_info''

satTopLevelExpr :: CoreExpr -> IdSet -> SatM (CoreExpr, IdSATInfo)
satTopLevelExpr expr interesting_ids = do
    (expr', sat_info_expr, expr_app) <- satExpr expr interesting_ids
    return (expr', finalizeApp expr_app sat_info_expr)

satExpr :: CoreExpr -> IdSet -> SatM (CoreExpr, IdSATInfo, Maybe IdAppInfo)
satExpr var@(Var v) interesting_ids = do
    let app_info = if v `elementOfUniqSet` interesting_ids
                   then Just (v, [])
                   else Nothing
    return (var, emptyIdSATInfo, app_info)

satExpr lit@(Lit _) _ =
    return (lit, emptyIdSATInfo, Nothing)

satExpr (Lam binders body) interesting_ids = do
    (body', sat_info, this_app) <- satExpr body interesting_ids
    return (Lam binders body', finalizeApp this_app sat_info, Nothing)

satExpr (App fn arg) interesting_ids = do
    (fn', sat_info_fn, fn_app) <- satExpr fn interesting_ids
    let satRemainder = boring fn' sat_info_fn
    case fn_app of
        Nothing -> satRemainder Nothing
        Just (fn_id, fn_app_info) ->
            -- TODO: remove this use of append somehow (use a data structure with O(1) append but a left-to-right kind of interface)
            let satRemainderWithStaticness arg_staticness = satRemainder $ Just (fn_id, fn_app_info ++ [arg_staticness])
            in case arg of
                Type t     -> satRemainderWithStaticness $ Static (TypeApp t)
                Coercion c -> satRemainderWithStaticness $ Static (CoApp c)
                Var v      -> satRemainderWithStaticness $ Static (VarApp v)
                _          -> satRemainderWithStaticness $ NotStatic
  where
    boring :: CoreExpr -> IdSATInfo -> Maybe IdAppInfo -> SatM (CoreExpr, IdSATInfo, Maybe IdAppInfo)
    boring fn' sat_info_fn app_info =
        do (arg', sat_info_arg, arg_app) <- satExpr arg interesting_ids
           let sat_info_arg' = finalizeApp arg_app sat_info_arg
               sat_info = mergeIdSATInfo sat_info_fn sat_info_arg'
           return (App fn' arg', sat_info, app_info)

satExpr (Case expr bndr ty alts) interesting_ids = do
    (expr', sat_info_expr, expr_app) <- satExpr expr interesting_ids
    let sat_info_expr' = finalizeApp expr_app sat_info_expr

    zipped_alts' <- mapM satAlt alts
    let (alts', sat_infos_alts) = unzip zipped_alts'
    return (Case expr' bndr ty alts', mergeIdSATInfo sat_info_expr' (mergeIdSATInfos sat_infos_alts), Nothing)
  where
    satAlt (con, bndrs, expr) = do
        (expr', sat_info_expr) <- satTopLevelExpr expr interesting_ids
        return ((con, bndrs, expr'), sat_info_expr)

satExpr (Let bind body) interesting_ids = do
    (body', sat_info_body, body_app) <- satExpr body interesting_ids
    (bind', sat_info_bind) <- satBind bind interesting_ids
    return (Let bind' body', mergeIdSATInfo sat_info_body sat_info_bind, body_app)

satExpr (Tick tickish expr) interesting_ids = do
    (expr', sat_info_expr, expr_app) <- satExpr expr interesting_ids
    return (Tick tickish expr', sat_info_expr, expr_app)

satExpr ty@(Type _) _ =
    return (ty, emptyIdSATInfo, Nothing)

satExpr co@(Coercion _) _ =
    return (co, emptyIdSATInfo, Nothing)

satExpr (Cast expr coercion) interesting_ids = do
    (expr', sat_info_expr, expr_app) <- satExpr expr interesting_ids
    return (Cast expr' coercion, sat_info_expr, expr_app)

{-
************************************************************************

                Static Argument Transformation Monad

************************************************************************
-}

type SatM result = UniqSM result

runSAT :: UniqSupply -> SatM a -> a
runSAT = initUs_

{-
************************************************************************

                Static Argument Transformation Monad

************************************************************************

To do the transformation, the game plan is to:

1. Create a small nonrecursive RHS that takes the
   original arguments to the function but discards
   the ones that are static and makes a call to the
   SATed version with the remainder. We intend that
   this will be inlined later, removing the overhead

2. Bind this nonrecursive RHS over the original body
   WITH THE SAME UNIQUE as the original body so that
   any recursive calls to the original now go via
   the small wrapper

3. Rebind the original function to a new one which contains
   our SATed function and just makes a call to it:
   we call the thing making this call the local body.
   In order to make 'saTransform' pure, we also give this
   function THE SAME UNIQUE as the original one.
   You can convince yourself below that that is fine.

Example: transform this

    map :: forall a b. (a->b) -> [a] -> [b]
    map = /\ab. \(f:a->b) (as:[a]) -> body[map]
to
    map :: forall a b. (a->b) -> [a] -> [b]
    map = /\ab. \(f:a->b) (as:[a]) ->
         letrec map' :: [a] -> [b]
                    -- The "worker function
                map' = \(as:[a]) ->
                         let map :: forall a' b'. (a -> b) -> [a] -> [b]
                                -- The "shadow function
                             map = /\a'b'. \(f':(a->b) (as:[a]).
                                   map' as
                         in body[map]
         in map' as

Note [Shadow binding]
~~~~~~~~~~~~~~~~~~~~~
The calls to the inner map inside body[map] should get inlined
by the local re-binding of 'map'.  We call this the "shadow binding".

But we can't use the original binder 'map' unchanged, because
it might be exported, in which case the shadow binding won't be
discarded as dead code after it is inlined.

So we use a hack: we make a new SysLocal binder with the *same* unique
as binder.  (Another alternative would be to reset the export flag.)

Note [Binder type capture]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice that in the inner map (the "shadow function"), the static arguments
are discarded -- it's as if they were underscores. Instead, mentions of
these arguments (notably in the types of dynamic arguments) are bound by
the *outer* lambdas of the main function. So we must make use Uniques for
the static arguments that do not capture variables mentioned in the types
of dynamic args. If we had a 'UniqSupply', we'd just give them fresh Uniques.
But we get by without: 'GHC.Builtin.Names.unboundKey' is a Unique that works
perfectly well for our purposes! Remember, they are dead anyway, so they can
shadow each other.

In the map example, the shadow function must set the unique
of the static type arguments a,b, giving a',b' (shadowing
each other), to ensure that in the \(as:[a]), the 'a' is
bound by the outer forall. We give f' 'unboundKey', too,
for consistency, but that doesn't matter either way because
static Id arguments aren't mentioned in the shadow binding
at all.

If we don't we get something like this:

[Exported]
[Arity 3]
GHC.Base.until =
  \ (@ a_aiK)
    (p_a6T :: a_aiK -> GHC.Types.Bool)
    (f_a6V :: a_aiK -> a_aiK)
    (x_a6X :: a_aiK) ->
    letrec {
      $suntil_s1aU :: a_aiK -> a_aiK
      []
      $suntil_s1aU =
        \ (x_a6X :: a_aiK) ->
          let {
            sat_shadow_r17 :: forall a_a3O.
                              (a_a3O -> GHC.Types.Bool) -> (a_a3O -> a_a3O) -> a_a3O -> a_a3O
            []
            sat_shadow_r17 =
              \ (@ a_aiK)
                (p_a6T :: a_aiK -> GHC.Types.Bool)
                (f_a6V :: a_aiK -> a_aiK)
                (x_a6X :: a_aiK) ->
                $suntil_s1aU x_a6X } in
          case p_a6T x_a6X of wild_X3y [ALWAYS Dead Nothing] {
            GHC.Types.False -> GHC.Base.until @ a_aiK p_a6T f_a6V (f_a6V x_a6X);
            GHC.Types.True -> x_a6X
          }; } in
    $suntil_s1aU x_a6X

Where sat_shadow has captured the type variables of x_a6X etc as it has a a_aiK
type argument. This is bad because it means the application $suntil_s1aU x_a6X
is not be well typed.
-}

saTransformMaybe :: Id -> Maybe SATInfo -> [Id] -> CoreExpr -> SatM CoreBind
saTransformMaybe binder maybe_arg_staticness rhs_binders rhs_body
  | Just arg_staticness <- maybe_arg_staticness
  , should_transform arg_staticness
  = do  { new_rhs <- return $ saTransform binder arg_staticness rhs_binders rhs_body
        ; return (NonRec binder new_rhs) }
  | otherwise
  = return (Rec [(binder, mkLams rhs_binders rhs_body)])
  where
    should_transform staticness = n_static_args > 1 -- THIS IS THE DECISION POINT
      where
        n_static_args = count isStaticValue staticness

saTransform :: Id -> [Staticness a] -> [Id] -> CoreExpr -> CoreExpr
-- Precondition: At least as many arg_staticness as rhs_binders
-- Precondition: At least one NotStatic
-- Precondition: Not all Static if rhs_body is unlifted
saTransform binder arg_staticness rhs_binders rhs_body
  = ASSERT2( arg_staticness `leLength` rhs_binders, ppr binder $$ ppr (mkStaticArgs arg_staticness) $$ ppr rhs_binders )
    ASSERT2( mkStaticArgs arg_staticness /= noStaticArgs, ppr binder $$ ppr rhs_binders )
    mk_new_rhs (map mk_shadow_lam_bndr binders_w_staticness)
  where

    -- Running example: foldr
    -- foldr \alpha \beta c n xs = e, for some e
    -- arg_staticness = [Static TypeApp, Static TypeApp, Static VarApp, Static VarApp, NonStatic]
    -- rhs_binders = [\alpha, \beta, c, n, xs]
    -- rhs_body = e

    binders_w_staticness = rhs_binders `zip` (arg_staticness ++ repeat NotStatic)
                                        -- Any extra args are assumed NotStatic

    non_static_args :: [Var]
            -- non_static_args = [xs]
            -- rhs_binders_without_type_capture = [\alpha', \beta', c, n, xs]
    non_static_args = [v | (v, NotStatic) <- binders_w_staticness]

    mk_shadow_lam_bndr (bndr, NotStatic) = bndr
    mk_shadow_lam_bndr (bndr, _        ) = setVarUnique bndr unboundKey
                                           -- See Note [Binder type capture]

    -- new_rhs = \alpha beta c n xs ->
    --           let $sfoldr = \xs -> let sat_shadow = \alpha' beta' c n xs ->
    --                                       $sfoldr xs
    --                                   in e
    --           in $sfoldr xs
    mk_new_rhs shadow_lam_bndrs
        = ASSERT2( not (isUnliftedType rec_body_ty), ppr binder $$ ppr rhs_body )
          mkLams rhs_binders $
          Let (Rec [(rec_body_bndr, rec_body)])
          local_body
        where
          uniq = idUnique binder

          local_body = mkVarApps (Var rec_body_bndr) non_static_args

          rec_body = mkLams non_static_args $
                     Let (NonRec shadow_bndr shadow_rhs) rhs_body

            -- See Note [Binder type capture]
          shadow_rhs = mkLams shadow_lam_bndrs local_body
            -- nonrec_rhs = \alpha' beta' c n xs -> $sfoldr xs


          occ_name = mkSpecOcc (getOccName binder)
          src_span = getSrcSpan (idName binder)
          rec_body_ty = exprType rec_body
          rec_body_bndr = mkUserLocal occ_name uniq Many rec_body_ty src_span
            -- rec_body_bndr = $sfoldr

            -- See Note [Shadow binding]; make a SysLocal
          shadow_bndr = mkSysLocal (occNameFS occ_name)
                                   uniq
                                   Many
                                   (exprType shadow_rhs)

isStaticValue :: Staticness App -> Bool
isStaticValue (Static (VarApp _)) = True
isStaticValue _                   = False

mkSatUnfolding :: TopLevelFlag -> Id -> CoreExpr -> Maybe Unfolding
mkSatUnfolding top_lvl id new_rhs
  | (lam_bndrs, lam_body) <- collectBinders new_rhs
  , static_args <- takeStaticArgs (length lam_bndrs) $ idStaticArgs id
  , static_args /= noStaticArgs
  , let unf_rhs = saTransform id (getStaticArgs static_args) lam_bndrs lam_body
  , let unf = mkCoreUnfolding InlineStable is_top_lvl unf_rhs guidance
  = Just unf
  | otherwise
  = Nothing
  where
    is_top_lvl = isTopLevel top_lvl
    !is_top_bottoming = is_top_lvl && isDeadEndId id
    opts              = defaultUnfoldingOpts
    guidance          = calcUnfoldingGuidance opts is_top_bottoming new_rhs
        -- NB: we calculate the guidance from the untransformed new_rhs!
        -- The SAT'd unfolding will introduce new let binds and lambdas that
        -- all go away, but will be counted by 'calcUnfoldingGuidance', which
        -- penalises SAT'd unfoldings too much.


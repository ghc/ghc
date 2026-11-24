module GHC.Tc.Gen.SourceClosure (publishCheckedSourceClosures) where

import GHC.Prelude

import Control.Applicative ((<|>))
import Control.Monad (guard, zipWithM)
import Control.Monad.Fix (mfix)
import Data.List (foldl', sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)

import GHC.Builtin.WiredIn.Types (manyDataConTy, unmatchableDataConTy)
import GHC.Core (CoreBind, CoreExpr)
import qualified GHC.Core as Core
import GHC.Core.Coercion.Axiom (CoAxBranch, ClosedTyFam(CTF), Role(Nominal))
import qualified GHC.Core.DataCon as DC
import GHC.Core.FamInstEnv (emptyFamInstEnvs, mkBranchedCoAxiom, mkCoAxBranch)
import GHC.Core.FVs (exprFreeVars, exprSomeFreeVarsList)
import GHC.Core.OpenWhnf (checkedMultiplicityCast)
import GHC.Core.Subst (extendTvSubst, mkEmptySubst, substExpr)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (Coercion(..), MCoercion(MRefl), Scaled(..), Type(TyVarTy), scaledThing)
import GHC.Core.TyCon
  ( FamTyConFlav(ClosedTypeFamilyTyCon), AlgTyConFlav(VanillaAlgTyCon)
  , TyCon, TyConBinder, Injectivity(NotInjective), mkAlgTyCon
  , mkLevPolyDataTyConRhs, mkTyConKind, mkAnonTyConBinder, mkNamedTyConBinders
  , mkFamilyTyCon, mkTyConKindWithMatchability, tyConDataCons, tyConKind, tyConName )
import GHC.Core.Type
  ( Kind, isLiftedTypeKind, liftedTypeKind, lookupTyVar, mkTyConApp, mkTyVarTy
  , mkTyVarTys, splitForAllTyVars, splitFunTys, splitTyConApp_maybe, substTyUnchecked, typeKind )
import GHC.Core.Unify (tcMatchTys)
import GHC.Core.Utils (exprType)
import GHC.Tc.Gen.SourceTable (hasSourceMethodRule, visibleSourceMethodRules)
import GHC.Tc.TyCl.Build (buildDataCon, newImplicitBinder, newTyConRepName)
import GHC.Tc.Utils.Instantiate (freshenTyVarBndrs)
import GHC.Tc.Utils.Monad (TcGblEnv(..), TcM, getGblEnv, newSysName)
import GHC.Types.Avail (availNames)
import GHC.Types.Id (Id, idDetails, idName, isDataConWorkId_maybe, isDataConWrapId_maybe)
import GHC.Types.Id.Info (IdDetails(CheckedClosureSourceTableId, VanillaId))
import GHC.Types.Id.Make (DataConBangOpts(FixedBangOpts))
import GHC.Types.Name
  ( Name, isExternalName, mkDataOcc, mkInstTyCoOcc, mkSourceTableOcc, mkTcOcc
  , mkTyVarOcc, nameModule_maybe, nameStableString, occNameString )
import GHC.Types.Name.Env (mkNameEnv)
import GHC.Types.Name.Set (elemNameSet, mkNameSet)
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Types.SrcLoc (noSrcSpan)
import GHC.Types.Tickish (GenTickish(Breakpoint))
import GHC.Types.TyThing (TyThing(AnId))
import GHC.Types.TypeEnv (TypeEnv, lookupTypeEnv)
import GHC.Types.Var
  ( ForAllTyFlag(Specified), TyVar, isCoVar, isId, isTyVar
  , Var, mkTyVar, mkTyVarBinders, setIdDetails, varType )
import GHC.Types.Var.Env (mkInScopeSet)
import GHC.Types.Var.Set (elemVarSet, mkVarSet, unionVarSet)

{- Note [Certifying a checked source closure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A rank-2 substitution callback can capture a term and can cross a
binder. Represent the supported callbacks as three private TypeData
constructors: replacement, one-binder shift, and lifting a callback
under a binder. Two nominal families interpret the closures over
term and variable constructors. The source owner names a third
family, which initializes the replacement closure.

Check the original and final Core before allocating the families.
A traversal must scrutinize its input, cover every constructor, and
rebuild each lazy field from a recursive call on the corresponding
original field. The call can use the original callback or a checked
extension. Check the kind of every generated branch against the
promoted constructor telescope. A neutral term blocks reduction.

Keep the actual owner and helper Names in IdDetails. Withhold
publication if the names are absent after Tidy or if a visible
rule can rewrite a checked function. See Note [Checked closure dependencies] in GHC.Tc.Gen.SourceTable.
-}

data FamilyInputs = FamilyInputs Id Kind Kind TyCon TyCon TyCon
                                  DC.DataCon DC.DataCon DC.DataCon [(DC.DataCon, [Bool])]

-- | Publish tables for eligible final Core bindings. The caller checks
-- the source-equation opt-in and excludes plugins and local user rules.
publishCheckedSourceClosures :: [CoreBind] -> [CoreBind] -> TypeEnv -> TcM ([Id], [TyCon])
publishCheckedSourceClosures original_binds final_binds final_types = do
    env <- getGblEnv
    visible <- visibleSourceMethodRules env
    let here = Just (tcg_mod env)
        exported = mkNameSet (concatMap availNames (tcg_exports env))
        original = Core.flattenBinds original_binds
        final = Core.flattenBinds final_binds
        lookupLocal name = case lookupTypeEnv final_types name of
          Just (AnId actual) | idName actual == name
                             , nameModule_maybe name == here -> Just actual
          _ -> Nothing
        candidate (old, original_rhs) = do
          let owner = idName old
          guard (isExternalName owner && owner `elemNameSet` exported)
          actual <- lookupLocal owner
          VanillaId <- pure (idDetails actual)
          original_shape <- familyInputs original old original_rhs
          final_rhs <- lookup actual final
          shape@(FamilyInputs _ _ _ term ext index _ _ _ _) <- familyInputs final actual final_rhs
          guard (sameShapes original_shape shape)
          guard (all ((== here) . nameModule_maybe . tyConName) [term, ext, index])
          let helpers = filter (/= owner) (reachable original old)
          helper_ids <- traverse lookupLocal helpers
          guard (not (null helpers) && all (not . hasSourceMethodRule visible) (actual:helper_ids))
          pure (actual, shape, helpers)
    accepted <- catMaybes <$> traverse publish (mapMaybe candidate original)
    pure (map fst accepted, concatMap snd accepted)
  where
    publish (owner, shape, helpers) = do
      generated <- allocateFamilies shape
      pure $ do
        (root, families) <- generated
        let new = setIdDetails owner (CheckedClosureSourceTableId (idName owner) root helpers)
        pure (new, families)

sameShapes :: FamilyInputs -> FamilyInputs -> Bool
sameShapes (FamilyInputs _ g a term ext index var hit miss rows)
           (FamilyInputs _ g' a' term' ext' index' var' hit' miss' rows') =
    g `eqType` g' && a `eqType` a' && term == term' && ext == ext' && index == index' &&
    var == var' && hit == hit' && miss == miss' && rows == rows'

familyInputs :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe FamilyInputs
familyInputs bindings owner rhs = do
    ([context, sort, result], [body, capture], call) <- function rhs
    (Core.Var traversal, [callback, Core.Var operand]) <- application call
    guard (operand == body)
    ([callback_sort], [argument], lookup_body) <- function callback
    let context_kind = varType context
        sort_kind = varType sort
    guard (all (eqType sort_kind) [varType result, varType callback_sort])
    (term, [body_ctx, TyVarTy body_sort]) <- splitTyConApp_maybe (varType body)
    (ext, [TyVarTy g, TyVarTy a]) <- splitTyConApp_maybe body_ctx
    (target, [TyVarTy capture_ctx, TyVarTy capture_sort]) <- splitTyConApp_maybe (varType capture)
    (index, [index_ctx, TyVarTy index_sort]) <- splitTyConApp_maybe (varType argument)
    guard (g == context && a == sort && body_sort == result &&
           target == term && capture_ctx == context && capture_sort == sort &&
           index_ctx `eqType` body_ctx && index_sort == callback_sort)
    [(first, first_bs, first_rhs), (second, second_bs, second_rhs)] <- caseOn argument lookup_body
    let index_cons = tyConDataCons index
    guard (first /= second && length index_cons == 2 &&
           first `elem` index_cons && second `elem` index_cons &&
           all lazyConstructor index_cons)
    (hit, miss, var) <-
        outcomes capture (first, first_bs, first_rhs) (second, second_bs, second_rhs) <|>
        outcomes capture (second, second_bs, second_rhs) (first, first_bs, first_rhs)
    guard (DC.dataConTyCon var == term)
    traversal_rhs <- lookup traversal bindings
    ([_, _, _], [sigma, scrutinee], traversal_case) <- function traversal_rhs
    rows <- caseOn scrutinee traversal_case
    [(var_bs, var_rhs)] <- pure [(bs, e) | (con, bs, e) <- rows, con == var]
    [field] <- pure (filter isValueVar var_bs)
    checked <- branchBody var_bs var_rhs
    (Core.Var use, [Core.Var x]) <- application checked
    guard (use == sigma && x == field)
    recipes <- checkedTraversals bindings traversal sigma var hit miss rows
    pure (FamilyInputs owner context_kind sort_kind term ext index var hit miss recipes)

-- Check both case traversals before emitting any nominal equations.
-- The recipe records, in field order, whether each recursive call
-- extends the original callback.
checkedTraversals :: [(Id, CoreExpr)]
                  -> Id -> Id
                  -> DC.DataCon -> DC.DataCon -> DC.DataCon
                  -> [(DC.DataCon, [Id], CoreExpr)]
                  -> Maybe [(DC.DataCon, [Bool])]
checkedTraversals bindings subst sigma var hit miss rows = do
    let constructors = tyConDataCons (DC.dataConTyCon var)
    guard (complete constructors rows)
    lifted <- traverse (checkedRow subst sigma) [row | row@(con, _, _) <- rows, con /= var]
    let (recipes, used_extenders) = unzip lifted
    extender <- allSame [v | group <- used_extenders, Just v <- group]
    extender_rhs <- lookup extender bindings
    rename <- checkedExtension bindings var hit miss extender_rhs
    rename_rhs <- lookup rename bindings
    (tvs, [rho, operand], renaming) <- function rename_rhs
    guard (length tvs == 3)
    rename_rows <- caseOn operand renaming
    guard (complete constructors rename_rows)
    [(var_binders, var_rhs)] <- pure [(bs, rhs) | (con, bs, rhs) <- rename_rows, con == var]
    [original] <- pure (filter isValueVar var_binders)
    var_result <- branchBody var_binders var_rhs
    (var_head, [renamed]) <- constructorCall var_result
    guard (var_head == var)
    (Core.Var callback, [Core.Var argument]) <- application renamed
    guard (callback == rho && argument == original)
    renamed_rows <- traverse (checkedRow rename rho)
                             [row | row@(con, _, _) <- rename_rows, con /= var]
    let (rename_recipes, rename_extenders) = unzip renamed_rows
    guard (sortOn (nameStableString . DC.dataConName . fst) recipes == sortOn (nameStableString . DC.dataConName . fst) rename_recipes)
    extend_rename <- allSame [v | group <- rename_extenders, Just v <- group]
    extend_rename_rhs <- lookup extend_rename bindings
    checkRenameExtension hit miss extend_rename_rhs
    pure recipes
  where
    complete constructors alts =
      let heads = [con | (con, _, _) <- alts]
      in length heads == length constructors && all (`elem` heads) constructors &&
         all lazyConstructor constructors

    allSame [] = Nothing
    allSame (v : vs) = v <$ guard (all (== v) vs)

-- Check every recursive call on exactly the original corresponding
-- field. The result keeps the same head and lazy field order.
checkedRow :: Id -> Id
           -> (DC.DataCon, [Id], CoreExpr)
           -> Maybe ((DC.DataCon, [Bool]), [Maybe Id])
checkedRow traversal callback (con, binders, rhs) = do
    result <- branchBody binders rhs
    (rebuilt, fields) <- constructorCall result
    let originals = filter isValueVar binders
    guard (rebuilt == con && length fields == length originals)
    owners <- zipWithM recursiveField originals fields
    pure ((con, map isJust owners), owners)
  where
    recursiveField original field = do
        (Core.Var owner, [captured, Core.Var subterm]) <- application field
        guard (traversal == owner && original == subterm &&
               isLiftedTypeKind (typeKind (varType original)))
        case captured of
            Core.Var other | callback == other -> pure Nothing
            _ -> Just <$> extendedCallback callback captured

extendedCallback :: Id -> CoreExpr -> Maybe Id
extendedCallback callback captured = do
    ([index], [], closure) <- function captured
    (Core.Var extender, args) <- pure (Core.collectArgs closure)
    [Core.Type _, Core.Type _, Core.Type _, Core.Var original,
      Core.Type (TyVarTy rest)] <- pure args
    guard (callback == original && index == rest)
    pure extender

-- Check the function used when a renaming crosses a binder.
checkRenameExtension :: DC.DataCon -> DC.DataCon -> CoreExpr -> Maybe ()
checkRenameExtension hit miss rhs = do
    let (binders, body) = Core.collectBinders (strip rhs)
    [g, d, b, capture, sort_var, argument] <- pure binders
    guard (all isTyVar [g,d,b,sort_var] && all isValueVar [capture,argument])
    rows <- caseOn argument body
    [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
    [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
    guard (length rows == 2 && all (not . isValueVar) hit_bs &&
           lazyConstructor hit && lazyConstructor miss)
    hit_body <- branchBody hit_bs hit_rhs
    (fresh, []) <- constructorCall hit_body
    guard (fresh == hit)
    [older] <- pure (filter isValueVar miss_bs)
    miss_body <- branchBody miss_bs miss_rhs
    (successor, [lookup_older]) <- constructorCall miss_body
    guard (successor == miss)
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    guard (lookup_capture == capture && input == older)

lazyConstructor :: DC.DataCon -> Bool
lazyConstructor con =
    not (any DC.isBanged (DC.dataConImplBangs con)) &&
    null (DC.dataConRepArgBinders con) &&
    null (DC.dataConStupidTheta con)

reachable :: [(Id, CoreExpr)] -> Id -> [Name]
reachable bindings root = sortOn nameStableString (map idName (walk [] [root]))
  where
    local = mkVarSet (map fst bindings)
    walk seen [] = seen
    walk seen (v:rest)
      | v `elem` seen = walk seen rest
      | Just body <- lookup v bindings =
          walk (v:seen) (exprSomeFreeVarsList (\x -> x `elemVarSet` local) body ++ rest)
      | otherwise = walk seen rest

-- A final simplifier can lift a closed constructor-only expression
-- into a local type abstraction. Check at most that one definition.
expandTypeHelper :: [(Id, CoreExpr)] -> CoreExpr -> CoreExpr
expandTypeHelper bindings rhs
  | (Core.Var helper, args) <- Core.collectArgs rhs
  , Just declaration <- lookup helper bindings
  , Just (tvs, [], body) <- function declaration
  , Just tys <- traverse typeArg args
  , length tvs == length tys
  , let in_scope = mkInScopeSet (foldl' unionVarSet (exprFreeVars body)
                                    (map (exprFreeVars . Core.Type) tys))
        substitution = foldl' (\sub (tv, ty) -> extendTvSubst sub tv ty)
                             (mkEmptySubst in_scope) (zip tvs tys)
        expanded = substExpr substitution body
  , exprType expanded `eqType` exprType rhs
  = expanded
  | otherwise = rhs
  where
    typeArg (Core.Type ty) = Just ty
    typeArg _ = Nothing

weakeningBody :: CoreExpr -> Maybe CoreExpr
weakeningBody (Core.Cast body co) = body <$ guard (checkedMultiplicityCast co)
weakeningBody body = Just body

checkedExtension :: [(Id, CoreExpr)]
                 -> DC.DataCon -> DC.DataCon -> DC.DataCon
                 -> CoreExpr -> Maybe Id
checkedExtension bindings var hit miss rhs = do
    let (binders, body) = Core.collectBinders (strip rhs)
    [g, d, b, capture, sort_var, argument] <- pure binders
    guard (all isTyVar [g,d,b,sort_var] && all isValueVar [capture,argument])
    rows <- caseOn argument body
    [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
    [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
    guard (length rows == 2 && all (not . isValueVar) hit_bs)
    hit_body <- branchBody hit_bs hit_rhs
    (term, [v]) <- constructorCall (expandTypeHelper bindings hit_body)
    (fresh, []) <- constructorCall v
    guard (term == var && fresh == hit)
    [older] <- pure (filter isValueVar miss_bs)
    miss_body <- branchBody miss_bs miss_rhs
    (Core.Var rename, [weakening, lookup_older]) <- application miss_body
    _ <- lookup rename bindings
    ([_], [], partial) <- weakeningBody weakening >>= function
    (shift, []) <- constructorCall partial
    guard (shift == miss)
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    guard (lookup_capture == capture && input == older)
    pure rename

constructorCall :: CoreExpr -> Maybe (DC.DataCon, [CoreExpr])
constructorCall rhs = do
    (Core.Var make, fields) <- application rhs
    con <- isDataConWorkId_maybe make <|> isDataConWrapId_maybe make
    pure (con, fields)

-- Observe only the original branch fields and proof variables.
outcomes :: Id
         -> (DC.DataCon, [Id], CoreExpr)
         -> (DC.DataCon, [Id], CoreExpr)
         -> Maybe (DC.DataCon, DC.DataCon, DC.DataCon)
outcomes capture (hit, hit_binders, hit_rhs) (miss, miss_binders, miss_rhs) = do
    guard (all (not . isValueVar) hit_binders)
    Core.Var selected <- branchBody hit_binders hit_rhs
    guard (selected == capture)
    [field] <- pure (filter isValueVar miss_binders)
    forwarded <- branchBody miss_binders miss_rhs
    (Core.Var make, [Core.Var x]) <- application forwarded
    guard (x == field)
    con <- isDataConWorkId_maybe make <|> isDataConWrapId_maybe make
    pure (hit, miss, con)

function :: CoreExpr -> Maybe ([Id], [Id], CoreExpr)
function expr = do
    let (binders, body) = Core.collectBinders (strip expr)
        (ty_binders, tm_binders) = span isTyVar binders
    guard (all isValueVar tm_binders)
    pure (ty_binders, tm_binders, strip body)

application :: CoreExpr -> Maybe (CoreExpr, [CoreExpr])
application expr = do
    let (head_expr, args) = Core.collectArgs (strip expr)
        (_, values) = span Core.isTyCoArg args
    guard (all (not . Core.isTyCoArg) values)
    pure (head_expr, map strip values)

caseOn :: Id -> CoreExpr
       -> Maybe [(DC.DataCon, [Id], CoreExpr)]
caseOn target expr = do
    Core.Case (Core.Var scrutinee) _ _ alts <- pure (strip expr)
    guard (target == scrutinee)
    traverse dataAlt alts
  where
    dataAlt (Core.Alt (Core.DataAlt con) bs rhs) = Just (con, bs, rhs)
    dataAlt _ = Nothing

branchBody :: [Id] -> CoreExpr -> Maybe CoreExpr
branchBody binders expr = case strip expr of
    Core.Cast body co
      | caseCoercion (filter isCoVar binders) co -> branchBody binders body
      | otherwise -> Nothing
    body -> Just body

caseCoercion :: [Var] -> Coercion -> Bool
caseCoercion scope co = case co of
    Refl _ -> True
    GRefl _ _ MRefl -> True
    CoVarCo cv -> cv `elem` scope
    TyConAppCo _ _ cs -> all (caseCoercion scope) cs
    SubCo c -> caseCoercion scope c
    SymCo c -> caseCoercion scope c
    SelCo _ c -> caseCoercion scope c
    _ -> False

strip :: CoreExpr -> CoreExpr
strip (Core.Tick Breakpoint{} expr) = strip expr
strip expr = expr

isValueVar :: Id -> Bool
isValueVar v = isId v && not (isCoVar v)

allocateFamilies :: FamilyInputs -> TcM (Maybe (TyCon, [TyCon]))
allocateFamilies (FamilyInputs owner context sort term ext lookup_tycon var hit miss recipes) = do
    (closure, constructors) <- allocateClosure owner context sort term ext
    case constructors of
      [replace, shift, lift] -> do
        apply_name <- localName "$apply:"
        lookup_name <- localName "$lookup:"
        apply_axiom <- newImplicitBinder apply_name mkInstTyCoOcc
        lookup_axiom <- newImplicitBinder lookup_name mkInstTyCoOcc
        root_name <- newImplicitBinder (idName owner) mkSourceTableOcc
        root_axiom <- newImplicitBinder root_name mkInstTyCoOcc
        g <- freshVar "g" context
        d <- freshVar "d" context
        a <- freshVar "a" sort
        b <- freshVar "b" sort
        let gk = mkTyVarTy g
            dk = mkTyVarTy d
            ak = mkTyVarTy a
            bk = mkTyVarTy b
            termAt p k = mkTyConApp term [p,k]
            ctxAt p k = mkTyConApp ext [p,k]
            lookupAt p k = mkTyConApp lookup_tycon [p,k]
            closureAt p q = mkTyConApp closure [p,q]
            ctx_g_a = ctxAt gk ak
            ctx_d_a = ctxAt dk ak
        cv <- freshVar "c" (closureAt gk dk)
        tree <- freshVar "t" (termAt gk bk)
        field <- freshVar "x" (lookupAt gk bk)
        carried <- freshVar "w" (termAt dk ak)
        body <- freshVar "n" (termAt ctx_g_a bk)
        arg <- freshVar "v" (termAt gk ak)
        x_d <- freshVar "older" (lookupAt dk bk)
        let prepare (ctor, exten) = do
              let (ctor_tvs, tel) = splitForAllTyVars (tyConKind (DC.promoteDataCon ctor))
                  (_, arg_kinds, res) = splitFunTys tel
              (renaming, tvs) <- freshenTyVarBndrs (map snd ctor_tvs)
              vars <- traverse (freshVar "arg" . substTyUnchecked renaming . scaledThing) arg_kinds
              let result = substTyUnchecked renaming res
              env <- freshVar "env" $ case splitTyConApp_maybe result of
                Just (_, [src, _]) -> closureAt src dk
                _ -> liftedTypeKind  -- Rejected by applyBranch.
              pure (ctor, exten, tvs, vars, result, env)
        apply_inputs <- traverse prepare ((var,[]) : recipes)
        let c = mkTyVarTy cv
            x = mkTyVarTy field
            w = mkTyVarTy carried
            outputs = termAt dk bk
            prefix = mkNamedTyConBinders Specified [g,d,b]
            apply_apply s t k env val = mkTyConApp apply [s,t,k,env,val]
            lookup_apply s t k env val = mkTyConApp lookup_fam [s,t,k,env,val]
            apply = mkFamily apply_name apply_axiom (prefix ++ map mkAnonTyConBinder [cv,tree])
                        outputs (fromMaybe [] apply_branches)
            lookup_fam = mkFamily lookup_name lookup_axiom (prefix ++ map mkAnonTyConBinder [cv,field])
                        outputs (fromMaybe [] lookup_branches)
            applyBranch (ctor, exten, tvs, vars, res, env_var) = do
                (actual_term, [src, sort_k]) <- splitTyConApp_maybe res
                guard (actual_term == term && (ctor == var || length exten == length vars))
                let fields = mkTyVarTys vars
                    lhs = mkTyConApp (DC.promoteDataCon ctor) (mkTyVarTys tvs ++ fields)
                    env = mkTyVarTy env_var
                    use lv value = do
                      (same, [source, k]) <- splitTyConApp_maybe (typeKind value)
                      guard (same == term)
                      if lv
                        then do
                          (scope, [outer, introduced]) <- splitTyConApp_maybe source
                          guard (scope == ext && outer `eqType` src)
                          lifted <- conAt lift (closureAt source (ctxAt dk introduced)) [env]
                          pure (apply_apply source (ctxAt dk introduced) k lifted value)
                        else do
                          guard (source `eqType` src)
                          pure (apply_apply src dk k env value)
                out <- if ctor == var
                         then do [lookup_var] <- pure fields
                                 pure (lookup_apply src dk sort_k env lookup_var)
                         else zipWithM use exten fields >>= conAt ctor (termAt dk sort_k)
                guard (typeKind lhs `eqType` res && typeKind out `eqType` termAt dk sort_k)
                pure (mkBranch (tvs ++ [d,env_var] ++ vars) [src,dk,sort_k,env,lhs] out)
            apply_branches = traverse applyBranch apply_inputs
            lookup_branches = do
                r <- conAt replace (closureAt ctx_d_a dk) [w]
                z_in <- conAt hit (lookupAt ctx_d_a ak) []
                s_in <- conAt miss (lookupAt ctx_d_a bk) [mkTyVarTy x_d]
                v_d <- conAt var (termAt dk bk) [mkTyVarTy x_d]
                s <- conAt shift (closureAt gk ctx_g_a) []
                s_out <- conAt miss (lookupAt ctx_g_a bk) [x]
                v_out <- conAt var (termAt ctx_g_a bk) [s_out]
                l <- conAt lift (closureAt ctx_g_a ctx_d_a) [c]
                z_g <- conAt hit (lookupAt ctx_g_a ak) []
                z_d <- conAt hit (lookupAt ctx_d_a ak) []
                v0 <- conAt var (termAt ctx_d_a ak) [z_d]
                sx <- conAt miss (lookupAt ctx_g_a bk) [x]
                s_d <- conAt shift (closureAt dk ctx_d_a) []
                let weaken = apply_apply dk ctx_d_a bk s_d (lookup_apply gk dk bk c x)
                pure [ mkBranch [d,a,carried] [ctx_d_a,dk,ak,r,z_in] w
                     , mkBranch [d,a,b,carried,x_d] [ctx_d_a,dk,bk,r,s_in] v_d
                     , mkBranch [g,a,b,field] [gk,ctx_g_a,bk,s,x] v_out
                     , mkBranch [g,d,a,cv] [ctx_g_a,ctx_d_a,ak,l,z_g] v0
                     , mkBranch [g,d,a,b,cv,field] [ctx_g_a,ctx_d_a,bk,l,sx] weaken ]
        pure $ do
            _ <- apply_branches
            _ <- lookup_branches
            r <- conAt replace (closureAt ctx_g_a gk) [mkTyVarTy arg]
            let result = termAt gk bk
                t = apply_apply ctx_g_a gk bk r (mkTyVarTy body)
                params = mkNamedTyConBinders Specified [g,a,b] ++ map mkAnonTyConBinder [body,arg]
                arguments = mkTyVarTys [g,a,b,body,arg]
                root = mkFamily root_name root_axiom params result
                          [mkBranch [g,a,b,body,arg] arguments t]
            pure (root, [root,closure,apply,lookup_fam])
      _ -> pure Nothing
  where
    localName prefix = newImplicitBinder (idName owner) (mkTcOcc . (prefix ++) . occNameString)

freshVar :: String -> Kind -> TcM TyVar
freshVar str kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc str)

-- Match the promoted telescope at the intended kind. The order of
-- source quantification can differ from the order of GADT parameters.
conAt :: DC.DataCon -> Type -> [Type] -> Maybe Type
conAt con result fields = do
    let promoted = DC.promoteDataCon con
        (annotated, telescope) = splitForAllTyVars (tyConKind promoted)
        (_, inputs, output) = splitFunTys telescope
    guard (length inputs == length fields)
    subst <- tcMatchTys (output : map scaledThing inputs) (result : map typeKind fields)
    args <- traverse (lookupTyVar subst . snd) annotated
    let value = mkTyConApp promoted (args ++ fields)
    value <$ guard (typeKind value `eqType` result)

mkFamily :: Name -> Name -> [TyConBinder] -> Kind -> [CoAxBranch] -> TyCon
mkFamily name axname binders result branches =
    let family = mkFamilyTyCon name (mkTyConKindWithMatchability unmatchableDataConTy binders result)
                    binders 0 result Nothing (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal axname family branches
    in family

mkBranch :: [TyVar] -> [Type] -> Type -> CoAxBranch
mkBranch vars lhs rhs =
    mkCoAxBranch vars [] [] lhs rhs (map (const Nominal) vars) noSrcSpan

allocateClosure :: Id -> Kind -> Kind -> TyCon -> TyCon
                -> TcM (TyCon, [DC.DataCon])
allocateClosure owner context_kind sort_kind term ext = do
    let tag prefix occ = prefix ++ occNameString occ
    closure_name <- newImplicitBinder (idName owner) (mkTcOcc . tag "$closure:")
    closure_rep <- newTyConRepName closure_name
    replace_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$replace:")
    shift_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$shift:")
    lift_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$lift:")
    let names = [replace_name, shift_name, lift_name]
        tags = mkNameEnv (zip names [1..])
        ordinary = DC.HsSrcBang NoSourceText DC.NoSrcUnpack DC.NoSrcStrict
    source <- freshVar "source" context_kind
    target <- freshVar "target" context_kind
    incoming <- freshVar "incoming" context_kind
    outgoing <- freshVar "outgoing" context_kind
    binder <- freshVar "binder" sort_kind
    let universals = [source, target]
        source_ty = mkTyVarTy source
        target_ty = mkTyVarTy target
        incoming_ty = mkTyVarTy incoming
        outgoing_ty = mkTyVarTy outgoing
        binder_ty = mkTyVarTy binder
        extend ctx = mkTyConApp ext [ctx, binder_ty]
        binders = map mkAnonTyConBinder universals
        kind = mkTyConKind binders liftedTypeKind
    closure <- mfix $ \result -> do
        let applied a b = mkTyConApp result [a,b]
            ctor name existentials user_tvs equalities fields res = do
              representative <- newTyConRepName name
              buildDataCon emptyFamInstEnvs (FixedBangOpts (map (const DC.HsLazy) fields))
                name False representative (map (const ordinary) fields) [] universals existentials
                (mkTyVarBinders Specified user_tvs) equalities []
                (map (Scaled manyDataConTy) fields) res result tags
        replace <- ctor replace_name [binder] [target,binder]
                     [DC.mkEqSpec source (extend target_ty)]
                     [mkTyConApp term [target_ty,binder_ty]] (applied (extend target_ty) target_ty)
        shift <- ctor shift_name [binder] [source,binder]
                   [DC.mkEqSpec target (extend source_ty)] [] (applied source_ty (extend source_ty))
        lift <- ctor lift_name [incoming,outgoing,binder] [incoming,outgoing,binder]
                  [DC.mkEqSpec source (extend incoming_ty), DC.mkEqSpec target (extend outgoing_ty)]
                  [applied incoming_ty outgoing_ty] (applied (extend incoming_ty) (extend outgoing_ty))
        pure (mkAlgTyCon closure_name kind binders 0 liftedTypeKind [Nominal,Nominal] Nothing []
                (mkLevPolyDataTyConRhs True True [replace,shift,lift]) (VanillaAlgTyCon closure_rep) True)
    pure (closure, tyConDataCons closure)

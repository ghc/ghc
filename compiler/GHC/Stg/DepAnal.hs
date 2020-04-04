{-# LANGUAGE CPP #-}

module GHC.Stg.DepAnal (depSortStgPgm) where

import GhcPrelude

--import GHC.Core    ( Tickish(Breakpoint) )
import GHC.Stg.Syntax
import GHC.Types.Id
import GHC.Types.Name (Name) --, nameIsLocalOrFrom)
import GHC.Types.Name.Env
import Outputable
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Types.Var.Set
import GHC.Types.Module (Module)
import Util

--import Data.Maybe ( mapMaybe )

import Data.Graph (SCC (..))



--------------------------------------------------------------------------------
-- * Dependency analysis

-- | Set of bound variables
type BVs = VarSet

-- | Set of free variables
type FVs = VarSet

newtype Env
  = Env
  { locals :: IdSet
  }

emptyEnv :: Env
emptyEnv = Env emptyVarSet

addLocals :: [Id] -> Env -> Env
addLocals bndrs env
  = env { locals = extendVarSetList (locals env) bndrs }

-- | This makes sure that only local, non-global free vars make it into the set.
mkFreeVarSet :: Env -> [Id] -> DIdSet
mkFreeVarSet env = mkDVarSet . filter (`elemVarSet` locals env)

-- | Dependency analysis and free variable annotations on STG terms.
--
-- Dependencies of a binding are just free variables in the binding. This
-- includes imported ids and ids in the current module. For recursive groups we
-- just return one set of free variables which is just the union of dependencies
-- of all bindings in the group.
--
-- Implementation: pass bound variables (BVs) to recursive calls, get free
-- variables (FVs) back. We ignore imported FVs as they do not change the
-- ordering but it improves performance.
--
annTopBindingsDeps :: Module -> [StgTopBinding] -> [(CgStgTopBinding, FVs)]
annTopBindingsDeps this_mod bs = map top_bind bs
  where
    top_bind :: StgTopBinding -> (CgStgTopBinding, FVs)
    top_bind (StgTopStringLit id bs) =
      (StgTopStringLit id bs, emptyVarSet)

    top_bind (StgTopLifted bs) =
       (StgTopLifted bs', fvs)
      where
       (bs', _dIdSet, fvs) = binding emptyEnv emptyDVarSet emptyVarSet bs

    binding :: Env -> DIdSet -> BVs -> StgBinding -> (CgStgBinding, DIdSet, FVs)
    binding env body_fv bounds (StgNonRec bndr r) =
        (StgNonRec bndr r', fvs, da_fvs)
      where
        -- See Note [Tracking local binders]
        (r', rhs_fvs, da_fvs) = rhs env bounds r
        fvs = delDVarSet body_fv bndr `unionDVarSet` rhs_fvs
    binding env body_fv bounds (StgRec bindings) =
        ( StgRec (zip bndrs rhss')
        , delDVarSetList (unionDVarSets (body_fv:rhs_fvss)) bndrs
        , unionVarSets da_fvss
        )
      where
        (bndrs, rhss) = unzip bindings
        bounds' = extendVarSetList bounds bndrs
        (rhss', rhs_fvss, da_fvss) = mapAndUnzip3 (rhs env bounds') rhss

    rhs :: Env -> BVs -> StgRhs -> (CgStgRhs, DIdSet, FVs)
    rhs env bounds (StgRhsClosure _ ccs uf bndrs body) =
        ( StgRhsClosure fvs ccs uf bndrs body'
        , fvs
        , da_fvs
        )
      where
        (body', body_fvs, da_fvs) = expr (addLocals bndrs env) (extendVarSetList bounds bndrs) body
        fvs = delDVarSetList body_fvs bndrs
    rhs env bounds (StgRhsCon ccs dc as) =
        ( StgRhsCon ccs dc as
        , fvs
        , da_fvs
        )
      where
        (fvs, da_fvs) = args env bounds as

    expr :: Env -> BVs -> StgExpr -> (CgStgExpr, DIdSet, FVs)
    expr env = go
      where
        go bounds (StgApp occ as) =
            ( StgApp occ as
            , unionDVarSet fvs (mkFreeVarSet env [occ])
            , da_fvs
            )
          where
            (fvs, da_fvs) = args env bounds as
        go bounds (StgLit lit) =
            (StgLit lit, emptyDVarSet, emptyVarSet)
        go bounds (StgConApp dc as tys) =
            (StgConApp dc as tys, fvs, da_fvs)
          where
            (fvs, da_fvs) = args env bounds as
        go bounds (StgOpApp op as ty) =
            (StgOpApp op as ty, fvs, da_fvs)
          where
            (fvs, da_fvs) = args env bounds as
        go _ lam@StgLam{} =
            pprPanic "annTopBindingsDeps" (text "Found lambda:" $$ ppr lam)
        go bounds (StgCase scrut bndr ty as) =
            ( StgCase scrut' bndr ty alts'
            , delDVarSet (unionDVarSet scrut_fvs alt_fvs) bndr
            , scrut_da_fvs `unionVarSet` alt_da_fvs
            )
          where
            (scrut', scrut_fvs, scrut_da_fvs) = go bounds scrut
            -- See Note [Tracking local binders]
            (alts', alt_fvs, alt_da_fvs) =
                alts (addLocals [bndr] env) (extendVarSet bounds bndr) as
        go bounds (StgLet ext bind body) = go_bind bounds (StgLet ext) bind body

        go_bind bounds dc bind body =
            ( dc bind' body'
            , fvs
            , da_bind_fvs `unionVarSet` da_body_fvs
            )
          where
            -- See Note [Tracking local binders]
            binders = bindersOf bind
            env' = addLocals binders env
            (body', body_fvs, da_body_fvs) =
                expr env' (extendVarSetList bounds binders) body
            (bind', fvs, da_bind_fvs) = binding env' body_fvs bounds bind


    alts :: Env -> BVs -> [StgAlt] -> ([CgStgAlt], DIdSet, FVs)
    alts env bounds as =
        ( as'
        , unionDVarSets alt_fvss
        , unionVarSets alt_da_fvss
        )
      where
        (as', alt_fvss, alt_da_fvss) = mapAndUnzip3 (alt env bounds) as

    alt :: Env -> BVs -> StgAlt -> (CgStgAlt, DIdSet, FVs)
    alt env bounds (con, bndrs, e) =
        ( (con, bndrs, e')
        , delDVarSetList rhs_fvs bndrs
        , da_fvs
        )
      where
        (e', rhs_fvs, da_fvs) = expr (addLocals bndrs env) (extendVarSetList bounds bndrs) e

    args :: Env -> BVs -> [StgArg] -> (DIdSet, FVs)
    args = undefined var

    var :: Env -> BVs -> Var -> (DIdSet, FVs)
    var = undefined this_mod

{-
    binding env body_fv bounds (StgRec pairs) =
      ( StgRec pairs'
      , fvs
      , unionVarSets $
          map (bind_non_rec (extendVarSetList bounds (map fst pairs))) pairs)
      where
        -- See Note [Tracking local binders]
        bndrs = map fst pairs
        (rhss, rhs_fvss) = mapAndUnzip (rhsFV env . snd) pairs
        pairs' = zip bndrs rhss
        fvs = delDVarSetList (unionDVarSets (body_fv:rhs_fvss)) bndrs

    rhsFV :: Env -> StgRhs -> (CgStgRhs, DIdSet)
    rhsFV env (StgRhsClosure _ ccs uf bndrs body)
      = (StgRhsClosure fvs ccs uf bndrs body', fvs)
      where
        -- See Note [Tracking local binders]
        (body', body_fvs) = exprFV (addLocals bndrs env) body
        fvs = delDVarSetList body_fvs bndrs
    rhsFV env (StgRhsCon ccs dc as) = (StgRhsCon ccs dc as, argsFV env as)

    exprFV :: Env -> StgExpr -> (CgStgExpr, DIdSet)
    exprFV env = go
      where
        go (StgApp occ as)
          = (StgApp occ as, unionDVarSet (argsFV env as) (mkFreeVarSet env [occ]))
        go (StgLit lit) = (StgLit lit, emptyDVarSet)
        go (StgConApp dc as tys) = (StgConApp dc as tys, argsFV env as)
        go (StgOpApp op as ty) = (StgOpApp op as ty, argsFV env as)
        go StgLam{} = pprPanic "StgFVs: StgLam" empty
        go (StgCase scrut bndr ty alts) = (StgCase scrut' bndr ty alts', fvs)
          where
            (scrut', scrut_fvs) = go scrut
            -- See Note [Tracking local binders]
            (alts', alt_fvss) = mapAndUnzip (altFV (addLocals [bndr] env)) alts
            alt_fvs = unionDVarSets alt_fvss
            fvs = delDVarSet (unionDVarSet scrut_fvs alt_fvs) bndr
        go (StgLet ext bind body) = go_bind (StgLet ext) bind body
        go (StgLetNoEscape ext bind body) = go_bind (StgLetNoEscape ext) bind body
        go (StgTick tick e) = (StgTick tick e', fvs')
          where
            (e', fvs) = go e
            fvs' = unionDVarSet (tickish tick) fvs
            tickish (Breakpoint _ ids) = mkDVarSet ids
            tickish _                  = emptyDVarSet
    
        go_bind dc bind body = (dc bind' body', fvs)
          where
            -- See Note [Tracking local binders]
            env' = addLocals (boundIds bind) env
            (body', body_fvs) = exprFV env' body
            (bind', fvs, _) = binding env' body_fvs bind

    argsFV :: Env -> [StgArg] -> DIdSet
    argsFV env = mkFreeVarSet env . mapMaybe f
      where
        f (StgVarArg occ) = Just occ
        f _               = Nothing

    bind_non_rec :: BVs -> (Id, StgRhs) -> FVs
    bind_non_rec bounds (_, r) =
        rhs bounds r

    rhs :: BVs -> StgRhs -> FVs
    rhs bounds (StgRhsClosure _ _ _ as e) =
      expr (extendVarSetList bounds as) e

    rhs bounds (StgRhsCon _ _ as) =
      args bounds as

    var :: BVs -> Var -> FVs
    var bounds v
      | not (elemVarSet v bounds)
      , nameIsLocalOrFrom this_mod (idName v)
      = unitVarSet v
      | otherwise
      = emptyVarSet

    arg :: BVs -> StgArg -> FVs
    arg bounds (StgVarArg v) = var bounds v
    arg _ StgLitArg{} = emptyVarSet

    args :: BVs -> [StgArg] -> FVs
    args bounds as = unionVarSets (map (arg bounds) as)

    expr :: BVs -> StgExpr -> FVs
    expr bounds (StgApp f as) =
      var bounds f `unionVarSet` args bounds as

    expr _ StgLit{} =
      emptyVarSet

    expr bounds (StgConApp _ as _) =
      args bounds as
    expr bounds (StgOpApp _ as _) =
      args bounds as
    expr _ lam@StgLam{} =
      pprPanic "annTopBindingsDeps" (text "Found lambda:" $$ ppr lam)
    expr bounds (StgCase scrut scrut_bndr _ as) =
      expr bounds scrut `unionVarSet`
        alts (extendVarSet bounds scrut_bndr) as
    expr bounds (StgLet _ bs e) =
      binding bounds bs `unionVarSet`
        expr (extendVarSetList bounds (bindersOf bs)) e
    expr bounds (StgLetNoEscape _ bs e) =
      binding bounds bs `unionVarSet`
        expr (extendVarSetList bounds (bindersOf bs)) e

    expr bounds (StgTick _ e) =
      expr bounds e

    alts :: BVs -> [StgAlt] -> FVs
    alts bounds = unionVarSets . map (alt bounds)

    alt :: BVs -> StgAlt -> FVs
    alt bounds (_, bndrs, e) =
      expr (extendVarSetList bounds bndrs) e

    altFV :: Env -> StgAlt -> (CgStgAlt, DIdSet)
    altFV env (con, bndrs, e) = ((con, bndrs, e'), fvs)
      where
        -- See Note [Tracking local binders]
        (e', rhs_fvs) = exprFV (addLocals bndrs env) e
        fvs = delDVarSetList rhs_fvs bndrs
-}

--------------------------------------------------------------------------------
-- * Dependency sorting

-- | Dependency sort a STG program so that dependencies come before uses.
depSortStgPgm :: Module -> [StgTopBinding] -> [CgStgTopBinding]
depSortStgPgm this_mod =
    {-# SCC "STG.depSort" #-}
    map fst . depSort . annTopBindingsDeps this_mod

-- | Sort free-variable-annotated STG bindings so that dependencies come before
-- uses.
depSort :: [(CgStgTopBinding, FVs)] -> [(CgStgTopBinding, FVs)]
depSort = concatMap get_binds . depAnal defs uses
  where
    uses, defs :: (CgStgTopBinding, FVs) -> [Name]

    -- TODO (osa): I'm unhappy about two things in this code:
    --
    --     * Why do we need Name instead of Id for uses and dependencies?
    --     * Why do we need a [Name] instead of `Set Name`? Surely depAnal
    --       doesn't need any ordering.

    uses (StgTopStringLit{}, _) = []
    uses (StgTopLifted{}, fvs)  = map idName (nonDetEltsUniqSet fvs)

    defs (bind, _) = map idName (bindersOfTop bind)

    get_binds (AcyclicSCC bind) =
      [bind]
    get_binds (CyclicSCC binds) =
      pprPanic "depSortStgBinds" (text "Found cyclic SCC:" $$ ppr binds)

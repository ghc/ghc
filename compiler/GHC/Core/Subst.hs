{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Utility functions on @Core@ syntax
-}

module GHC.Core.Subst (
        -- * Main data types
        Subst(..), -- Implementation exported for supercompiler's Renaming.hs only
        TvSubstEnv, IdSubstEnv, InScopeSet,

        -- ** Substituting into expressions and related types
        deShadowBinds, substRuleInfo, substRulesForImportedIds,
        substTyUnchecked, substCo, substExpr, substExprSC, substBind, substBindSC,
        substUnfolding, substUnfoldingSC,
        lookupIdSubst, lookupIdSubst_maybe, substIdType, substIdOcc,
        substTickish, substDVarSet, substIdInfo,

        -- ** Operations on substitutions
        emptySubst, mkEmptySubst, mkTCvSubst, mkOpenSubst, isEmptySubst,
        extendIdSubst, extendIdSubstList, extendTCvSubst, extendTvSubstList,
        extendIdSubstWithClone,
        extendSubst, extendSubstList, extendSubstWithVar,
        extendSubstInScope, extendSubstInScopeList, extendSubstInScopeSet,
        isInScope, setInScope, getSubstInScope,
        extendTvSubst, extendCvSubst,
        delBndr, delBndrs, zapSubst,

        -- ** Substituting and cloning binders
        substBndr, substBndrs, substRecBndrs, substTyVarBndr, substCoVarBndr,
        cloneBndr, cloneBndrs, cloneIdBndr, cloneIdBndrs, cloneRecIdBndrs,

    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.FVs
import GHC.Core.Seq
import GHC.Core.Utils

        -- We are defining local versions
import GHC.Core.Type hiding ( substTy )
import GHC.Core.Coercion
    ( tyCoFVsOfCo, mkCoVarCo, substCoVarBndr )

import GHC.Types.Var.Set
import GHC.Types.Var.Env as InScopeSet
import GHC.Types.Id
import GHC.Types.Name     ( Name )
import GHC.Types.Var
import GHC.Types.Tickish
import GHC.Types.Id.Info
import GHC.Types.Unique.Supply

import GHC.Builtin.Names
import GHC.Data.Maybe

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Functor.Identity (Identity (..))
import Data.List (mapAccumL)

{-
************************************************************************
*                                                                      *
\subsection{Substitutions}
*                                                                      *
************************************************************************
-}

{-
Note [Extending the IdSubstEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We make a different choice for Ids than we do for TyVars.

For TyVars, see Note [Extending the TvSubstEnv and CvSubstEnv] in GHC.Core.TyCo.Subst.

For Ids, we have a different invariant
        The IdSubstEnv is extended *only* when the Unique on an Id changes
        Otherwise, we just extend the InScopeSet

In consequence:

* If all subst envs are empty, substExpr would be a
  no-op, so substExprSC ("short cut") does nothing.

  However, substExpr still goes ahead and substitutes.  Reason: we may
  want to replace existing Ids with new ones from the in-scope set, to
  avoid space leaks.

* In substIdBndr, we extend the IdSubstEnv only when the unique changes

* If the CvSubstEnv, TvSubstEnv and IdSubstEnv are all empty,
  substExpr does nothing (Note that the above rule for substIdBndr
  maintains this property.  If the incoming envts are both empty, then
  substituting the type and IdInfo can't change anything.)

* In lookupIdSubst, we *must* look up the Id in the in-scope set, because
  it may contain non-trivial changes.  Example:
        (/\a. \x:a. ...x...) Int
  We extend the TvSubstEnv with [a |-> Int]; but x's unique does not change
  so we only extend the in-scope set.  Then we must look up in the in-scope
  set when we find the occurrence of x.

* The requirement to look up the Id in the in-scope set means that we
  must NOT take no-op short cut when the IdSubst is empty.
  We must still look up every Id in the in-scope set.

* (However, we don't need to do so for expressions found in the IdSubst
  itself, whose range is assumed to be correct wrt the in-scope set.)

Why do we make a different choice for the IdSubstEnv than the
TvSubstEnv and CvSubstEnv?

* For Ids, we change the IdInfo all the time (e.g. deleting the
  unfolding), and adding it back later, so using the TyVar convention
  would entail extending the substitution almost all the time

* The simplifier wants to look up in the in-scope set anyway, in case it
  can see a better unfolding from an enclosing case expression

* For TyVars, only coercion variables can possibly change, and they are
  easy to spot
-}

----------------------------

-- We keep GHC.Core.Subst separate from GHC.Core.TyCo.Subst to avoid creating
-- circular dependencies. Functions in this file that don't depend on
-- the definition of CoreExpr can be moved to GHC.Core.TyCo.Subst, as long
-- as it does not require importing too many additional hs-boot files and
-- cause a significant drop in performance.

-- | Add a substitution for an 'Id' to the 'Subst': you must ensure that the in-scope set is
-- such that TyCoSubst Note [The substitution invariant]
-- holds after extending the substitution like this
extendIdSubst :: Subst -> Id -> CoreExpr -> Subst
-- ToDo: add an ASSERT that fvs(subst-result) is already in the in-scope set
extendIdSubst (Subst in_scope ids tvs cvs) v r
  = assertPpr (isNonCoVarId v) (ppr v $$ ppr r) $
    Subst in_scope (extendVarEnv ids v r) tvs cvs

extendIdSubstWithClone :: Subst -> Id -> Id -> Subst
extendIdSubstWithClone (Subst in_scope ids tvs cvs) v v'
  = assertPpr (isNonCoVarId v) (ppr v $$ ppr v') $
    Subst (extendInScopeSetSet in_scope new_in_scope)
          (extendVarEnv ids v (varToCoreExpr v')) tvs cvs
    where
      new_in_scope = tyCoVarsOfType (varType v') `extendVarSet` v'

-- | Adds multiple 'Id' substitutions to the 'Subst': see also 'extendIdSubst'
extendIdSubstList :: Subst -> [(Id, CoreExpr)] -> Subst
extendIdSubstList (Subst in_scope ids tvs cvs) prs
  = assert (all (isNonCoVarId . fst) prs) $
    Subst in_scope (extendVarEnvList ids prs) tvs cvs

-- | Add a substitution appropriate to the thing being substituted
--   (whether an expression, type, or coercion). See also
--   'extendIdSubst', 'extendTvSubst', 'extendCvSubst'
extendSubst :: Subst -> Var -> CoreArg -> Subst
extendSubst subst var arg
  = case arg of
      Type ty     -> assert (isTyVar var) $ extendTvSubst subst var ty
      Coercion co -> assert (isCoVar var) $ extendCvSubst subst var co
      _           -> assert (isId    var) $ extendIdSubst subst var arg

extendSubstWithVar :: Subst -> Var -> Var -> Subst
extendSubstWithVar subst v1 v2
  | isTyVar v1 = assert (isTyVar v2) $ extendTvSubst subst v1 (mkTyVarTy v2)
  | isCoVar v1 = assert (isCoVar v2) $ extendCvSubst subst v1 (mkCoVarCo v2)
  | otherwise  = assert (isId    v2) $ extendIdSubst subst v1 (Var v2)

-- | Add a substitution as appropriate to each of the terms being
--   substituted (whether expressions, types, or coercions). See also
--   'extendSubst'.
extendSubstList :: Subst -> [(Var,CoreArg)] -> Subst
extendSubstList subst []              = subst
extendSubstList subst ((var,rhs):prs) = extendSubstList (extendSubst subst var rhs) prs

-- | Find the substitution for an 'Id' in the 'Subst'
-- The Id should not be a CoVar
lookupIdSubst :: HasDebugCallStack => Subst -> Id -> CoreExpr
lookupIdSubst (Subst in_scope ids _ _) v
  | assertPpr (isId v && not (isCoVar v)) (ppr v)
    not (isLocalId v)                   = Var v
  | Just e  <- lookupVarEnv ids       v = e
  | Just v' <- lookupInScope in_scope v = Var v'
        -- Vital! See Note [Extending the IdSubstEnv]
        -- If v isn't in the InScopeSet, we panic, because
        -- it's a bad bug and we really want to know
  | otherwise = pprPanic "lookupIdSubst" (ppr v $$ ppr in_scope)

lookupIdSubst_maybe :: HasDebugCallStack => Subst -> Id -> Maybe CoreExpr
-- Just look up in the substitution; do not check the in-scope set
lookupIdSubst_maybe (Subst _ ids _ _) v
  = assertPpr (isId v && not (isCoVar v)) (ppr v) $
    lookupVarEnv ids v

delBndr :: Subst -> Var -> Subst
delBndr (Subst in_scope ids tvs cvs) v
  | isCoVar v = Subst in_scope ids tvs (delVarEnv cvs v)
  | isTyVar v = Subst in_scope ids (delVarEnv tvs v) cvs
  | otherwise = Subst in_scope (delVarEnv ids v) tvs cvs

delBndrs :: Subst -> [Var] -> Subst
delBndrs (Subst in_scope ids tvs cvs) vs
  = Subst in_scope (delVarEnvList ids vs) (delVarEnvList tvs vs) (delVarEnvList cvs vs)
      -- Easiest thing is just delete all from all!

-- | Simultaneously substitute for a bunch of variables
--   No left-right shadowing
--   ie the substitution for   (\x \y. e) a1 a2
--      so neither x nor y scope over a1 a2
mkOpenSubst :: InScopeSet -> [(Var,CoreArg)] -> Subst
mkOpenSubst in_scope pairs = Subst in_scope
                                   (mkVarEnv [(id,e)  | (id, e) <- pairs, isId id])
                                   (mkVarEnv [(tv,ty) | (tv, Type ty) <- pairs])
                                   (mkVarEnv [(v,co)  | (v, Coercion co) <- pairs])

------------------------------

{-
************************************************************************
*                                                                      *
        Substituting expressions
*                                                                      *
************************************************************************
-}

substExprSC :: HasDebugCallStack => Subst -> CoreExpr -> CoreExpr
-- Just like substExpr, but a no-op if the substitution is empty
-- Note that this does /not/ replace occurrences of free vars with
-- their canonical representatives in the in-scope set
substExprSC subst orig_expr
  | isEmptySubst subst = orig_expr
  | otherwise          = -- pprTrace "enter subst-expr" (doc $$ ppr orig_expr) $
                         substExpr subst orig_expr

-- | substExpr applies a substitution to an entire 'CoreExpr'. Remember,
-- you may only apply the substitution /once/:
-- See Note [Substitutions apply only once] in "GHC.Core.TyCo.Subst"
--
-- Do *not* attempt to short-cut in the case of an empty substitution!
-- See Note [Extending the IdSubstEnv]
substExpr :: HasDebugCallStack => Subst -> CoreExpr -> CoreExpr
   -- HasDebugCallStack so we can track failures in lookupIdSubst
substExpr subst expr
  = go expr
  where
    go (Var v)         = lookupIdSubst subst v
    go (Type ty)       = Type (substTyUnchecked subst ty)
    go (Coercion co)   = Coercion (substCo subst co)
    go (Lit lit)       = Lit lit
    go (App fun arg)   = App (go fun) (go arg)
    go (Tick tickish e) = mkTick (substTickish subst tickish) (go e)
    go (Cast e co)     = Cast (go e) (substCo subst co)
       -- Do not optimise even identity coercions
       -- Reason: substitution applies to the LHS of RULES, and
       --         if you "optimise" an identity coercion, you may
       --         lose a binder. We optimise the LHS of rules at
       --         construction time

    go (Lam bndr body) = Lam bndr' (substExpr subst' body)
                       where
                         (subst', bndr') = substBndr subst bndr

    go (Let bind body) = Let bind' (substExpr subst' body)
                       where
                         (subst', bind') = substBind subst bind

    go (Case scrut bndr ty alts) = Case (go scrut) bndr' (substTyUnchecked subst ty) (map (go_alt subst') alts)
                                 where
                                 (subst', bndr') = substBndr subst bndr

    go_alt subst (Alt con bndrs rhs) = Alt con bndrs' (substExpr subst' rhs)
                                 where
                                   (subst', bndrs') = substBndrs subst bndrs

-- | Apply a substitution to an entire 'CoreBind', additionally returning an updated 'Subst'
-- that should be used by subsequent substitutions.
substBind, substBindSC :: HasDebugCallStack => Subst -> CoreBind -> (Subst, CoreBind)

substBindSC subst bind    -- Short-cut if the substitution is empty
  | not (isEmptySubst subst)
  = substBind subst bind
  | otherwise
  = case bind of
       NonRec bndr rhs -> (subst', NonRec bndr' rhs)
          where
            (subst', bndr') = substBndr subst bndr
       Rec pairs -> (subst', Rec (bndrs' `zip` rhss'))
          where
            (bndrs, rhss)    = unzip pairs
            (subst', bndrs') = substRecBndrs subst bndrs
            rhss' | isEmptySubst subst'
                  = rhss
                  | otherwise
                  = map (substExpr subst') rhss

substBind subst (NonRec bndr rhs)
  = (subst', NonRec bndr' (substExpr subst rhs))
  where
    (subst', bndr') = substBndr subst bndr

substBind subst (Rec pairs)
   = (subst', Rec (bndrs' `zip` rhss'))
   where
       (bndrs, rhss)    = unzip pairs
       (subst', bndrs') = substRecBndrs subst bndrs
       rhss' = map (substExpr subst') rhss

-- | De-shadowing the program is sometimes a useful pre-pass. It can be done simply
-- by running over the bindings with an empty substitution, because substitution
-- returns a result that has no-shadowing guaranteed.
--
-- (Actually, within a single /type/ there might still be shadowing, because
-- 'substTy' is a no-op for the empty substitution, but that's probably OK.)
--
-- [Aug 09] This function is not used in GHC at the moment, but seems so
--          short and simple that I'm going to leave it here
deShadowBinds :: CoreProgram -> CoreProgram
deShadowBinds binds = snd (mapAccumL substBind emptySubst binds)

{-
************************************************************************
*                                                                      *
        Substituting binders
*                                                                      *
************************************************************************

Remember that substBndr and friends are used when doing expression
substitution only.  Their only business is substitution, so they
preserve all IdInfo (suitably substituted).  For example, we *want* to
preserve occ info in rules.
-}

-- | Substitutes a 'Var' for another one according to the 'Subst' given, returning
-- the result and an updated 'Subst' that should be used by subsequent substitutions.
-- 'IdInfo' is preserved by this process, although it is substituted into appropriately.
substBndr :: Subst -> Var -> (Subst, Var)
substBndr subst bndr
  | isTyVar bndr  = substTyVarBndr subst bndr
  | isCoVar bndr  = substCoVarBndr subst bndr
  | otherwise     = substIdBndr (text "var-bndr") subst subst bndr

-- | Applies 'substBndr' to a number of 'Var's, accumulating a new 'Subst' left-to-right
substBndrs :: Traversable f => Subst -> f Var -> (Subst, f Var)
substBndrs = mapAccumL substBndr
{-# INLINE substBndrs #-}

-- | Substitute in a mutually recursive group of 'Id's
substRecBndrs :: Traversable f => Subst -> f Id -> (Subst, f Id)
substRecBndrs subst bndrs
  = (new_subst, new_bndrs)
  where         -- Here's the reason we need to pass rec_subst to subst_id
    (new_subst, new_bndrs) = mapAccumL (substIdBndr (text "rec-bndr") new_subst) subst bndrs
{-# SPECIALIZE substRecBndrs :: Subst -> [Id] -> (Subst, [Id]) #-}
{-# SPECIALIZE substRecBndrs :: Subst -> Identity Id -> (Subst, Identity Id) #-}

substIdBndr :: SDoc
            -> Subst            -- ^ Substitution to use for the IdInfo
            -> Subst -> Id      -- ^ Substitution and Id to transform
            -> (Subst, Id)      -- ^ Transformed pair
                                -- NB: unfolding may be zapped

substIdBndr _doc rec_subst subst@(Subst in_scope env tvs cvs) old_id
  = -- pprTrace "substIdBndr" (doc $$ ppr old_id $$ ppr in_scope) $
    (Subst new_in_scope new_env tvs cvs, new_id)
  where
    id1 = uniqAway in_scope old_id      -- id1 is cloned if necessary
    id2 | no_type_change = id1
        | otherwise      = updateIdTypeAndMult (substTyUnchecked subst) id1

    old_ty = idType old_id
    old_w = idMult old_id
    no_type_change = (isEmptyVarEnv tvs && isEmptyVarEnv cvs) ||
                     (noFreeVarsOfType old_ty && noFreeVarsOfType old_w)

        -- new_id has the right IdInfo
        -- The lazy-set is because we're in a loop here, with
        -- rec_subst, when dealing with a mutually-recursive group
    !new_id = maybeModifyIdInfo mb_new_info id2
    mb_new_info = substIdInfo rec_subst id2 (idInfo id2)
        -- NB: unfolding info may be zapped

        -- Extend the substitution if the unique has changed
        -- See the notes with substTyVarBndr for the delVarEnv
    !new_in_scope = in_scope `InScopeSet.extendInScopeSet` new_id
        -- Forcing new_in_scope improves T9675 by 1.7%
    !new_env | no_change = delVarEnv env old_id
             | otherwise = extendVarEnv env old_id (Var new_id)

    no_change = id1 == old_id
        -- See Note [Extending the IdSubstEnv]
        -- it's /not/ necessary to check mb_new_info and no_type_change

{-
Now a variant that unconditionally allocates a new unique.
It also unconditionally zaps the OccInfo.
-}

-- | Very similar to 'substBndr', but it always allocates a new 'Unique' for
-- each variable in its output.  It substitutes the IdInfo though.
-- Discards non-Stable unfoldings
cloneIdBndr :: Subst -> UniqSupply -> Id -> (Subst, Id)
cloneIdBndr subst us old_id
  = clone_id subst subst (old_id, uniqFromSupply us)

-- | Applies 'cloneIdBndr' to a number of 'Id's, accumulating a final
-- substitution from left to right
-- Discards non-Stable unfoldings
cloneIdBndrs :: Subst -> UniqSupply -> [Id] -> (Subst, [Id])
cloneIdBndrs subst us ids
  = mapAccumL (clone_id subst) subst (ids `zip` uniqsFromSupply us)

cloneBndrs :: MonadUnique m => Subst -> [Var] -> m (Subst, [Var])
-- Works for all kinds of variables (typically case binders)
-- not just Ids
cloneBndrs subst vs
  = do us <- getUniquesM
       pure $ mapAccumL (\subst (v, u) -> cloneBndr subst u v) subst (vs `zip` us)

cloneBndr :: Subst -> Unique -> Var -> (Subst, Var)
cloneBndr subst uniq v
  | isTyVar v = cloneTyVarBndr subst v uniq
  | otherwise = clone_id subst subst (v,uniq)  -- Works for coercion variables too

-- | Clone a mutually recursive group of 'Id's
cloneRecIdBndrs :: MonadUnique m => Subst -> [Id] -> m (Subst, [Id])
cloneRecIdBndrs subst ids
  = do us <- getUniquesM
       let (subst', ids') = mapAccumL (clone_id subst') subst (ids `zip` us)
       pure (subst', ids')

-- Just like substIdBndr, except that it always makes a new unique
-- It is given the unique to use
-- Discards non-Stable unfoldings
clone_id    :: Subst                    -- Substitution for the IdInfo
            -> Subst -> (Id, Unique)    -- Substitution and Id to transform
            -> (Subst, Id)              -- Transformed pair

clone_id rec_subst subst@(Subst in_scope idvs tvs cvs) (old_id, uniq)
  = (Subst new_in_scope new_idvs tvs new_cvs, new_id)
  where
    id1     = setVarUnique old_id uniq
    id2     = substIdType subst id1
    !new_id = maybeModifyIdInfo (substIdInfo rec_subst id2 (idInfo old_id)) id2
    !new_in_scope = in_scope `InScopeSet.extendInScopeSet` new_id
        -- Forcing new_in_scope improves T9675 by 1.7%
    (!new_idvs, !new_cvs) | isCoVar old_id = (idvs, extendVarEnv cvs old_id (mkCoVarCo new_id))
                          | otherwise      = (extendVarEnv idvs old_id (Var new_id), cvs)

{-
************************************************************************
*                                                                      *
                Types and Coercions
*                                                                      *
************************************************************************
-}

{-
************************************************************************
*                                                                      *
\section{IdInfo substitution}
*                                                                      *
************************************************************************
-}

substIdType :: Subst -> Id -> Id
substIdType subst@(Subst _ _ tv_env cv_env) id
  | (isEmptyVarEnv tv_env && isEmptyVarEnv cv_env)
    || (noFreeVarsOfType old_ty && noFreeVarsOfType old_w) = id
  | otherwise   =
      updateIdTypeAndMult (substTyUnchecked subst) id
        -- The tyCoVarsOfType is cheaper than it looks
        -- because we cache the free tyvars of the type
        -- in a Note in the id's type itself
  where
    old_ty = idType id
    old_w  = idMult id

------------------
-- | Substitute into some 'IdInfo' with regard to the supplied new 'Id'.
-- Discards unfoldings, unless they are Stable
substIdInfo :: Subst -> Id -> IdInfo -> Maybe IdInfo
substIdInfo subst new_id info
  | nothing_to_do = Nothing
  | otherwise     = Just (info `setRuleInfo`      substRuleInfo subst new_id old_rules
                               `setUnfoldingInfo` substUnfolding subst old_unf)
  where
    old_rules     = ruleInfo info
    old_unf       = realUnfoldingInfo info
    nothing_to_do = isEmptyRuleInfo old_rules && not (hasCoreUnfolding old_unf)

------------------
-- | Substitutes for the 'Id's within an unfolding
-- NB: substUnfolding /discards/ any unfolding without
--     without a Stable source.  This is usually what we want,
--     but it may be a bit unexpected
substUnfolding, substUnfoldingSC :: Subst -> Unfolding -> Unfolding
        -- Seq'ing on the returned Unfolding is enough to cause
        -- all the substitutions to happen completely

substUnfoldingSC subst unf       -- Short-cut version
  | isEmptySubst subst = unf
  | otherwise          = substUnfolding subst unf

substUnfolding subst df@(DFunUnfolding { df_bndrs = bndrs, df_args = args })
  = df { df_bndrs = bndrs', df_args = args' }
  where
    (subst',bndrs') = substBndrs subst bndrs
    args'           = map (substExpr subst') args

substUnfolding subst unf@(CoreUnfolding { uf_tmpl = tmpl, uf_src = src })
  -- Retain stable unfoldings
  | not (isStableSource src)  -- Zap an unstable unfolding, to save substitution work
  = NoUnfolding
  | otherwise                 -- But keep a stable one!
  = seqExpr new_tmpl `seq`
    unf { uf_tmpl = new_tmpl }
  where
    new_tmpl = substExpr subst tmpl

substUnfolding _ unf = unf      -- NoUnfolding, OtherCon

------------------
substIdOcc :: Subst -> Id -> Id
-- These Ids should not be substituted to non-Ids
substIdOcc subst v = case lookupIdSubst subst v of
                        Var v' -> v'
                        other  -> pprPanic "substIdOcc" (vcat [ppr v <+> ppr other, ppr subst])

------------------
-- | Substitutes for the 'Id's within the 'RuleInfo' given the new function 'Id'
substRuleInfo :: Subst -> Id -> RuleInfo -> RuleInfo
substRuleInfo subst new_id (RuleInfo rules rhs_fvs)
  = RuleInfo (map (substRule subst subst_ru_fn) rules)
                  (substDVarSet subst rhs_fvs)
  where
    subst_ru_fn = const (idName new_id)

------------------
substRulesForImportedIds :: Subst -> [CoreRule] -> [CoreRule]
substRulesForImportedIds subst rules
  = map (substRule subst not_needed) rules
  where
    not_needed name = pprPanic "substRulesForImportedIds" (ppr name)

------------------
substRule :: Subst -> (Name -> Name) -> CoreRule -> CoreRule

-- The subst_ru_fn argument is applied to substitute the ru_fn field
-- of the rule:
--    - Rules for *imported* Ids never change ru_fn
--    - Rules for *local* Ids are in the IdInfo for that Id,
--      and the ru_fn field is simply replaced by the new name
--      of the Id
substRule _ _ rule@(BuiltinRule {}) = rule
substRule subst subst_ru_fn rule@(Rule { ru_bndrs = bndrs, ru_args = args
                                       , ru_fn = fn_name, ru_rhs = rhs
                                       , ru_local = is_local })
  = rule { ru_bndrs = bndrs'
         , ru_fn    = if is_local
                        then subst_ru_fn fn_name
                        else fn_name
         , ru_args  = map (substExpr subst') args
         , ru_rhs   = substExpr subst' rhs }
           -- Do NOT optimise the RHS (previously we did simplOptExpr here)
           -- See Note [Substitute lazily]
  where
    (subst', bndrs') = substBndrs subst bndrs

------------------
substDVarSet :: HasDebugCallStack => Subst -> DVarSet -> DVarSet
substDVarSet subst@(Subst _ _ tv_env cv_env) fvs
  = mkDVarSet $ fst $ foldr subst_fv ([], emptyVarSet) $ dVarSetElems fvs
  where
  subst_fv :: Var -> ([Var], VarSet) -> ([Var], VarSet)
  subst_fv fv acc
     | isTyVar fv
     , let fv_ty = lookupVarEnv tv_env fv `orElse` mkTyVarTy fv
     = tyCoFVsOfType fv_ty (const True) emptyVarSet $! acc
     | isCoVar fv
     , let fv_co = lookupVarEnv cv_env fv `orElse` mkCoVarCo fv
     = tyCoFVsOfCo fv_co (const True) emptyVarSet $! acc
     | otherwise
     , let fv_expr = lookupIdSubst subst fv
     = exprFVs fv_expr (const True) emptyVarSet $! acc

------------------
-- | Drop free vars from the breakpoint if they have a non-variable substitution.
substTickish :: Subst -> CoreTickish -> CoreTickish
substTickish subst (Breakpoint ext n ids modl)
   = Breakpoint ext n (mapMaybe do_one ids) modl
 where
    do_one = getIdFromTrivialExpr_maybe . lookupIdSubst subst

substTickish _subst other = other

{- Note [Substitute lazily]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The functions that substitute over IdInfo must be pretty lazy, because
they are knot-tied by substRecBndrs.

One case in point was #10627 in which a rule for a function 'f'
referred to 'f' (at a different type) on the RHS.  But instead of just
substituting in the rhs of the rule, we were calling simpleOptExpr, which
looked at the idInfo for 'f'; result <<loop>>.

In any case we don't need to optimise the RHS of rules, or unfoldings,
because the simplifier will do that.

Another place this went wrong was in `substRuleInfo`, which would immediately force
the lazy call to substExpr, which led to an infinite loop (as reported by #20112).

This time the call stack looked something like:

* `substRecBndrs`
* `substIdBndr`
* `substIdInfo`
* `substRuleInfo`
* `substRule`
* `substExpr`
* `mkTick`
* `isSaturatedConApp`
* Look at `IdInfo` for thing we are currently substituting because the rule is attached to `transpose` and mentions it in the `RHS` of the rule.

and the rule was

{-# RULES
"transpose/overlays1" forall xs. transpose (overlays1 xs) = overlays1 (fmap transpose xs) #-}

This rule was attached to `transpose`, but also mentions itself in the RHS so we have
to be careful to not force the `IdInfo` for transpose when dealing with the RHS of the rule.



Note [substTickish]
~~~~~~~~~~~~~~~~~~~~~~
A Breakpoint contains a list of Ids.  What happens if we ever want to
substitute an expression for one of these Ids?

First, we ensure that we only ever substitute trivial expressions for
these Ids, by marking them as NoOccInfo in the occurrence analyser.
Then, when substituting for the Id, we unwrap any type applications
and abstractions to get back to an Id, with getIdFromTrivialExpr.

Second, we have to ensure that we never try to substitute a literal
for an Id in a breakpoint.  We ensure this by never storing an Id with
an unlifted type in a Breakpoint - see GHC.HsToCore.Ticks.mkTickish.
Breakpoints can't handle free variables with unlifted types anyway.

These measures are only reliable with unoptimized code.
Since we can now enable optimizations for GHCi with
@-fno-unoptimized-core-for-interpreter -O@, nontrivial expressions can be
substituted, e.g. by specializations.
Therefore we resort to discarding free variables from breakpoints when this
situation occurs.
-}

{-
Note [Worker inlining]
~~~~~~~~~~~~~~~~~~~~~~
A worker can get substituted away entirely.
        - it might be trivial
        - it might simply be very small
We do not treat an InlWrapper as an 'occurrence' in the occurrence
analyser, so it's possible that the worker is not even in scope any more.

In all these cases we simply drop the special case, returning to
InlVanilla.  The WARN is just so I can see if it happens a lot.
-}

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998


This module contains "tidying" code for *nested* expressions, bindings, rules.
The code for *top-level* bindings is in TidyPgm.
-}

{-# LANGUAGE CPP #-}
module CoreTidy (
        tidyExpr, tidyVarOcc, tidyRule, tidyRules, tidyUnfolding
    ) where

#include "HsVersions.h"

import GhcPrelude

import CoreSyn
import CoreSeq ( seqUnfolding )
import CoreArity
import Id
import IdInfo
import Demand ( zapUsageEnvSig )
import Type( tidyType, tidyTyCoVarBndr )
import Coercion( tidyCo )
import Var
import VarEnv
import UniqFM
import Name hiding (tidyNameOcc)
import SrcLoc
import Maybes
import Data.List

{-
************************************************************************
*                                                                      *
\subsection{Tidying expressions, rules}
*                                                                      *
************************************************************************
-}

tidyBind :: TidyEnv
         -> CoreBind
         ->  (TidyEnv, CoreBind)

tidyBind env (NonRec bndr rhs)
  = tidyLetBndr env env (bndr,rhs) =: \ (env', bndr') ->
    (env', NonRec bndr' (tidyExpr env' rhs))

tidyBind env (Rec prs)
  = let
       (env', bndrs') = mapAccumL (tidyLetBndr env') env prs
    in
    map (tidyExpr env') (map snd prs)   =: \ rhss' ->
    (env', Rec (zip bndrs' rhss'))


------------  Expressions  --------------
tidyExpr :: TidyEnv -> CoreExpr -> CoreExpr
tidyExpr env (Var v)       = Var (tidyVarOcc env v)
tidyExpr env (Type ty)     = Type (tidyType env ty)
tidyExpr env (Coercion co) = Coercion (tidyCo env co)
tidyExpr _   (Lit lit)     = Lit lit
tidyExpr env (App f a)     = App (tidyExpr env f) (tidyExpr env a)
tidyExpr env (Tick t e)    = Tick (tidyTickish env t) (tidyExpr env e)
tidyExpr env (Cast e co)   = Cast (tidyExpr env e) (tidyCo env co)

tidyExpr env (Let b e)
  = tidyBind env b      =: \ (env', b') ->
    Let b' (tidyExpr env' e)

tidyExpr env (Case e b ty alts)
  = tidyBndr env b  =: \ (env', b) ->
    Case (tidyExpr env e) b (tidyType env ty)
         (map (tidyAlt env') alts)

tidyExpr env (Lam b e)
  = tidyBndr env b      =: \ (env', b) ->
    Lam b (tidyExpr env' e)

------------  Case alternatives  --------------
tidyAlt :: TidyEnv -> CoreAlt -> CoreAlt
tidyAlt env (con, vs, rhs)
  = tidyBndrs env vs    =: \ (env', vs) ->
    (con, vs, tidyExpr env' rhs)

------------  Tickish  --------------
tidyTickish :: TidyEnv -> Tickish Id -> Tickish Id
tidyTickish env (Breakpoint ix ids) = Breakpoint ix (map (tidyVarOcc env) ids)
tidyTickish _   other_tickish       = other_tickish

------------  Rules  --------------
tidyRules :: TidyEnv -> [CoreRule] -> [CoreRule]
tidyRules _   [] = []
tidyRules env (rule : rules)
  = tidyRule env rule           =: \ rule ->
    tidyRules env rules         =: \ rules ->
    (rule : rules)

tidyRule :: TidyEnv -> CoreRule -> CoreRule
tidyRule _   rule@(BuiltinRule {}) = rule
tidyRule env rule@(Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs,
                          ru_fn = fn, ru_rough = mb_ns })
  = tidyBndrs env bndrs         =: \ (env', bndrs) ->
    map (tidyExpr env') args    =: \ args ->
    rule { ru_bndrs = bndrs, ru_args = args,
           ru_rhs   = tidyExpr env' rhs,
           ru_fn    = tidyNameOcc env fn,
           ru_rough = map (fmap (tidyNameOcc env')) mb_ns }

{-
************************************************************************
*                                                                      *
\subsection{Tidying non-top-level binders}
*                                                                      *
************************************************************************
-}

tidyNameOcc :: TidyEnv -> Name -> Name
-- In rules and instances, we have Names, and we must tidy them too
-- Fortunately, we can lookup in the VarEnv with a name
tidyNameOcc (_, var_env) n = case lookupUFM var_env n of
                                Nothing -> n
                                Just v  -> idName v

tidyVarOcc :: TidyEnv -> Var -> Var
tidyVarOcc (_, var_env) v = lookupVarEnv var_env v `orElse` v

-- tidyBndr is used for lambda and case binders
tidyBndr :: TidyEnv -> Var -> (TidyEnv, Var)
tidyBndr env var
  | isTyCoVar var = tidyTyCoVarBndr env var
  | otherwise     = tidyIdBndr env var

tidyBndrs :: TidyEnv -> [Var] -> (TidyEnv, [Var])
tidyBndrs env vars = mapAccumL tidyBndr env vars

-- Non-top-level variables, not covars
tidyIdBndr :: TidyEnv -> Id -> (TidyEnv, Id)
tidyIdBndr env@(tidy_env, var_env) id
  = -- Do this pattern match strictly, otherwise we end up holding on to
    -- stuff in the OccName.
    case tidyOccName tidy_env (getOccName id) of { (tidy_env', occ') ->
    let
        -- Give the Id a fresh print-name, *and* rename its type
        -- The SrcLoc isn't important now,
        -- though we could extract it from the Id
        --
        ty'      = tidyType env (idType id)
        name'    = mkInternalName (idUnique id) occ' noSrcSpan
        id'      = mkLocalIdWithInfo name' ty' new_info
        var_env' = extendVarEnv var_env id id'

        -- Note [Tidy IdInfo]
        new_info = vanillaIdInfo `setOccInfo` occInfo old_info
                                 `setUnfoldingInfo` new_unf
                                  -- see Note [Preserve OneShotInfo]
                                 `setOneShotInfo` oneShotInfo old_info
        old_info = idInfo id
        old_unf  = unfoldingInfo old_info
        new_unf | isEvaldUnfolding old_unf = evaldUnfolding
                | otherwise                = noUnfolding
          -- See Note [Preserve evaluatedness]
    in
    ((tidy_env', var_env'), id')
   }

tidyLetBndr :: TidyEnv         -- Knot-tied version for unfoldings
            -> TidyEnv         -- The one to extend
            -> (Id, CoreExpr) -> (TidyEnv, Var)
-- Used for local (non-top-level) let(rec)s
-- Just like tidyIdBndr above, but with more IdInfo
tidyLetBndr rec_tidy_env env@(tidy_env, var_env) (id,rhs)
  = case tidyOccName tidy_env (getOccName id) of { (tidy_env', occ') ->
    let
        ty'      = tidyType env (idType id)
        name'    = mkInternalName (idUnique id) occ' noSrcSpan
        details  = idDetails id
        id'      = mkLocalVar details name' ty' new_info
        var_env' = extendVarEnv var_env id id'

        -- Note [Tidy IdInfo]
        -- We need to keep around any interesting strictness and
        -- demand info because later on we may need to use it when
        -- converting to A-normal form.
        -- eg.
        --      f (g x),  where f is strict in its argument, will be converted
        --      into  case (g x) of z -> f z  by CorePrep, but only if f still
        --      has its strictness info.
        --
        -- Similarly for the demand info - on a let binder, this tells
        -- CorePrep to turn the let into a case.
        -- But: Remove the usage demand here
        --      (See Note [Zapping DmdEnv after Demand Analyzer] in WorkWrap)
        --
        -- Similarly arity info for eta expansion in CorePrep
        --
        -- Set inline-prag info so that we preseve it across
        -- separate compilation boundaries
        old_info = idInfo id
        new_info = vanillaIdInfo
                    `setOccInfo`        occInfo old_info
                    `setArityInfo`      exprArity rhs
                    `setStrictnessInfo` zapUsageEnvSig (strictnessInfo old_info)
                    `setDemandInfo`     demandInfo old_info
                    `setInlinePragInfo` inlinePragInfo old_info
                    `setUnfoldingInfo`  new_unf

        new_unf | isStableUnfolding old_unf = tidyUnfolding rec_tidy_env old_unf old_unf
                | isEvaldUnfolding  old_unf = evaldUnfolding
                                              -- See Note [Preserve evaluatedness]
                | otherwise                 = noUnfolding
        old_unf = unfoldingInfo old_info
    in
    ((tidy_env', var_env'), id') }

------------ Unfolding  --------------
tidyUnfolding :: TidyEnv -> Unfolding -> Unfolding -> Unfolding
tidyUnfolding tidy_env df@(DFunUnfolding { df_bndrs = bndrs, df_args = args }) _
  = df { df_bndrs = bndrs', df_args = map (tidyExpr tidy_env') args }
  where
    (tidy_env', bndrs') = tidyBndrs tidy_env bndrs

tidyUnfolding tidy_env
              unf@(CoreUnfolding { uf_tmpl = unf_rhs, uf_src = src })
              unf_from_rhs
  | isStableSource src
  = seqIt $ unf { uf_tmpl = tidyExpr tidy_env unf_rhs }    -- Preserves OccInfo
    -- This seqIt avoids a space leak: otherwise the uf_is_value,
    -- uf_is_conlike, ... fields may retain a reference to the
    -- pre-tidied expression forever (ToIface doesn't look at them)

  | otherwise
  = unf_from_rhs
  where seqIt unf = seqUnfolding unf `seq` unf
tidyUnfolding _ unf _ = unf     -- NoUnfolding or OtherCon

{-
Note [Tidy IdInfo]
~~~~~~~~~~~~~~~~~~
All nested Ids now have the same IdInfo, namely vanillaIdInfo, which
should save some space; except that we preserve occurrence info for
two reasons:

  (a) To make printing tidy core nicer

  (b) Because we tidy RULES and InlineRules, which may then propagate
      via --make into the compilation of the next module, and we want
      the benefit of that occurrence analysis when we use the rule or
      or inline the function.  In particular, it's vital not to lose
      loop-breaker info, else we get an infinite inlining loop

Note that tidyLetBndr puts more IdInfo back.

Note [Preserve evaluatedness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data T = MkT !Bool
  ....(case v of MkT y ->
       let z# = case y of
                  True -> 1#
                  False -> 2#
       in ...)

The z# binding is ok because the RHS is ok-for-speculation,
but Lint will complain unless it can *see* that.  So we
preserve the evaluated-ness on 'y' in tidyBndr.

(Another alternative would be to tidy unboxed lets into cases,
but that seems more indirect and surprising.)

Note [Preserve OneShotInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We keep the OneShotInfo because we want it to propagate into the interface.
Not all OneShotInfo is determined by a compiler analysis; some is added by a
call of GHC.Exts.oneShot, which is then discarded before the end of the
optimisation pipeline, leaving only the OneShotInfo on the lambda. Hence we
must preserve this info in inlinings. See Note [The oneShot function] in MkId.

This applies to lambda binders only, hence it is stored in IfaceLamBndr.
-}

(=:) :: a -> (a -> b) -> b
m =: k = m `seq` k m

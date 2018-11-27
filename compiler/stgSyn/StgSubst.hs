{-# LANGUAGE CPP #-}

module StgSubst where

#include "HsVersions.h"

import GhcPrelude

import Id
import VarEnv
import Control.Monad.Trans.State.Strict
import Outputable
import Util

-- | A renaming substitution from 'Id's to 'Id's. Like 'RnEnv2', but not
-- maintaining pairs of substitutions. Like @"CoreSubst".'CoreSubst.Subst'@, but
-- with the domain being 'Id's instead of entire 'CoreExpr'.
data Subst = Subst InScopeSet IdSubstEnv

type IdSubstEnv = IdEnv Id

-- | @emptySubst = 'mkEmptySubst' 'emptyInScopeSet'@
emptySubst :: Subst
emptySubst = mkEmptySubst emptyInScopeSet

-- | Constructs a new 'Subst' assuming the variables in the given 'InScopeSet'
-- are in scope.
mkEmptySubst :: InScopeSet -> Subst
mkEmptySubst in_scope = Subst in_scope emptyVarEnv

-- | Substitutes an 'Id' for another one according to the 'Subst' given in a way
-- that avoids shadowing the 'InScopeSet', returning the result and an updated
-- 'Subst' that should be used by subsequent substitutions.
substBndr :: Id -> Subst -> (Id, Subst)
substBndr id (Subst in_scope env)
  = (new_id, Subst new_in_scope new_env)
  where
    new_id = uniqAway in_scope id
    no_change = new_id == id -- in case nothing shadowed
    new_in_scope = in_scope `extendInScopeSet` new_id
    new_env
      | no_change = delVarEnv env id
      | otherwise = extendVarEnv env id new_id

-- | @substBndrs = runState . traverse (state . substBndr)@
substBndrs :: Traversable f => f Id -> Subst -> (f Id, Subst)
substBndrs = runState . traverse (state . substBndr)

-- | Substitutes an occurrence of an identifier for its counterpart recorded
-- in the 'Subst'.
lookupIdSubst :: HasCallStack => Id -> Subst -> Id
lookupIdSubst id (Subst in_scope env)
  | not (isLocalId id) = id
  | Just id' <- lookupVarEnv env id = id'
  | Just id' <- lookupInScope in_scope id = id'
  | otherwise = WARN( True, text "StgSubst.lookupIdSubst" <+> ppr id $$ ppr in_scope)
                id

-- | Substitutes an occurrence of an identifier for its counterpart recorded
-- in the 'Subst'. Does not generate a debug warning if the identifier to
-- to substitute wasn't in scope.
noWarnLookupIdSubst :: HasCallStack => Id -> Subst -> Id
noWarnLookupIdSubst id (Subst in_scope env)
  | not (isLocalId id) = id
  | Just id' <- lookupVarEnv env id = id'
  | Just id' <- lookupInScope in_scope id = id'
  | otherwise = id

-- | Add the 'Id' to the in-scope set and remove any existing substitutions for
-- it.
extendInScope :: Id -> Subst -> Subst
extendInScope id (Subst in_scope env) = Subst (in_scope `extendInScopeSet` id) env

-- | Add a substitution for an 'Id' to the 'Subst': you must ensure that the
-- in-scope set is such that TyCORep Note [The substitution invariant]
-- holds after extending the substitution like this.
extendSubst :: Id -> Id -> Subst -> Subst
extendSubst id new_id (Subst in_scope env)
  = ASSERT2( new_id `elemInScopeSet` in_scope, ppr id <+> ppr new_id $$ ppr in_scope )
    Subst in_scope (extendVarEnv env id new_id)

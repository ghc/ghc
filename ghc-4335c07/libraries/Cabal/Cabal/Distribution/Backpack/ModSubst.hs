{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}

-- | A type class 'ModSubst' for objects which can have 'ModuleSubst'
-- applied to them.
--
-- See also <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>

module Distribution.Backpack.ModSubst (
    ModSubst(..),
) where

import Prelude ()
import Distribution.Compat.Prelude hiding (mod)

import Distribution.ModuleName

import Distribution.Backpack

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Applying module substitutions to semantic objects.
class ModSubst a where
    -- In notation, substitution is postfix, which implies
    -- putting it on the right hand side, but for partial
    -- application it's more convenient to have it on the left
    -- hand side.
    modSubst :: OpenModuleSubst -> a -> a

instance ModSubst OpenModule where
    modSubst subst (OpenModule cid mod_name) = OpenModule (modSubst subst cid) mod_name
    modSubst subst mod@(OpenModuleVar mod_name)
        | Just mod' <- Map.lookup mod_name subst = mod'
        | otherwise = mod

instance ModSubst OpenUnitId where
    modSubst subst (IndefFullUnitId cid insts) = IndefFullUnitId cid (modSubst subst insts)
    modSubst _subst uid = uid

instance ModSubst (Set ModuleName) where
    modSubst subst reqs
        = Set.union (Set.difference reqs (Map.keysSet subst))
                    (openModuleSubstFreeHoles subst)

-- Substitutions are functorial.  NB: this means that
-- there is an @instance 'ModSubst' 'ModuleSubst'@!
instance ModSubst a => ModSubst (Map k a) where
    modSubst subst = fmap (modSubst subst)
instance ModSubst a => ModSubst [a] where
    modSubst subst = fmap (modSubst subst)
instance ModSubst a => ModSubst (k, a) where
    modSubst subst (x,y) = (x, modSubst subst y)

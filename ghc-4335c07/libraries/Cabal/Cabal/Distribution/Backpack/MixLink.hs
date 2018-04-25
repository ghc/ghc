{-# LANGUAGE NondecreasingIndentation #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.MixLink (
    mixLink,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding (mod)

import Distribution.Backpack
import Distribution.Backpack.UnifyM
import Distribution.Backpack.FullUnitId
import Distribution.Backpack.ModuleScope

import qualified Distribution.Utils.UnionFind as UnionFind
import Distribution.ModuleName
import Distribution.Text
import Distribution.Types.ComponentId

import Text.PrettyPrint
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Foldable as F

-----------------------------------------------------------------------
-- Linking

-- | Given to scopes of provisions and requirements, link them together.
mixLink :: [ModuleScopeU s] -> UnifyM s (ModuleScopeU s)
mixLink scopes = do
    let provs = Map.unionsWith (++) (map fst scopes)
        -- Invariant: any identically named holes refer to same mutable cell
        reqs  = Map.unionsWith (++) (map snd scopes)
        filled = Map.intersectionWithKey linkProvision provs reqs
    F.sequenceA_ filled
    let remaining = Map.difference reqs filled
    return (provs, remaining)

-- | Link a list of possibly provided modules to a single
-- requirement.  This applies a side-condition that all
-- of the provided modules at the same name are *actually*
-- the same module.
linkProvision :: ModuleName
              -> [ModuleWithSourceU s] -- provs
              -> [ModuleWithSourceU s] -- reqs
              -> UnifyM s [ModuleWithSourceU s]
linkProvision mod_name ret@(prov:provs) (req:reqs) = do
    -- TODO: coalesce all the non-unifying modules together
    forM_ provs $ \prov' -> do
        -- Careful: read it out BEFORE unifying, because the
        -- unification algorithm preemptively unifies modules
        mod  <- convertModuleU (unWithSource prov)
        mod' <- convertModuleU (unWithSource prov')
        r <- unify prov prov'
        case r of
            Just () -> return ()
            Nothing -> do
                addErr $
                  text "Ambiguous module" <+> quotes (disp mod_name) $$
                  text "It could refer to" <+>
                    ( text "  " <+> (quotes (disp mod)  $$ in_scope_by (getSource prov)) $$
                      text "or" <+> (quotes (disp mod') $$ in_scope_by (getSource prov')) ) $$
                  link_doc
    mod <- convertModuleU (unWithSource prov)
    req_mod <- convertModuleU (unWithSource req)
    self_cid <- fmap unify_self_cid getUnifEnv
    case mod of
      OpenModule (IndefFullUnitId cid _) _
        | cid == self_cid -> addErr $
            text "Cannot instantiate requirement" <+> quotes (disp mod_name) <+>
                in_scope_by (getSource req) $$
            text "with locally defined module" <+> in_scope_by (getSource prov) $$
            text "as this would create a cyclic dependency, which GHC does not support." $$
            text "Try moving this module to a separate library, e.g.," $$
            text "create a new stanza: library 'sublib'."
      _ -> return ()
    r <- unify prov req
    case r of
        Just () -> return ()
        Nothing -> do
            -- TODO: Record and report WHERE the bad constraint came from
            addErr $ text "Could not instantiate requirement" <+> quotes (disp mod_name) $$
                     nest 4 (text "Expected:" <+> disp mod $$
                             text "Actual:  " <+> disp req_mod) $$
                     parens (text "This can occur if an exposed module of" <+>
                             text "a libraries shares a name with another module.") $$
                     link_doc
    return ret
  where
    unify s1 s2 = tryM $ addErrContext short_link_doc
                       $ unifyModule (unWithSource s1) (unWithSource s2)
    in_scope_by s = text "brought into scope by" <+> dispModuleSource s
    short_link_doc = text "While filling requirement" <+> quotes (disp mod_name)
    link_doc = text "While filling requirements of" <+> reqs_doc
    reqs_doc
      | null reqs = dispModuleSource (getSource req)
      | otherwise =  (       text "   " <+> dispModuleSource (getSource req)  $$
                      vcat [ text "and" <+> dispModuleSource (getSource r) | r <- reqs])
linkProvision _ _ _ = error "linkProvision"



-----------------------------------------------------------------------
-- The unification algorithm

-- This is based off of https://gist.github.com/amnn/559551517d020dbb6588
-- which is a translation from Huet's thesis.

unifyUnitId :: UnitIdU s -> UnitIdU s -> UnifyM s ()
unifyUnitId uid1_u uid2_u
    | uid1_u == uid2_u = return ()
    | otherwise = do
        xuid1 <- liftST $ UnionFind.find uid1_u
        xuid2 <- liftST $ UnionFind.find uid2_u
        case (xuid1, xuid2) of
            (UnitIdThunkU u1, UnitIdThunkU u2)
                | u1 == u2  -> return ()
                | otherwise ->
                    failWith $ hang (text "Couldn't match unit IDs:") 4
                               (text "   " <+> disp u1 $$
                                text "and" <+> disp u2)
            (UnitIdThunkU uid1, UnitIdU _ cid2 insts2)
                -> unifyThunkWith cid2 insts2 uid2_u uid1 uid1_u
            (UnitIdU _ cid1 insts1, UnitIdThunkU uid2)
                -> unifyThunkWith cid1 insts1 uid1_u uid2 uid2_u
            (UnitIdU _ cid1 insts1, UnitIdU _ cid2 insts2)
                -> unifyInner cid1 insts1 uid1_u cid2 insts2 uid2_u

unifyThunkWith :: ComponentId
               -> Map ModuleName (ModuleU s)
               -> UnitIdU s
               -> DefUnitId
               -> UnitIdU s
               -> UnifyM s ()
unifyThunkWith cid1 insts1 uid1_u uid2 uid2_u = do
    db <- fmap unify_db getUnifEnv
    let FullUnitId cid2 insts2' = expandUnitId db uid2
    insts2 <- convertModuleSubst insts2'
    unifyInner cid1 insts1 uid1_u cid2 insts2 uid2_u

unifyInner :: ComponentId
           -> Map ModuleName (ModuleU s)
           -> UnitIdU s
           -> ComponentId
           -> Map ModuleName (ModuleU s)
           -> UnitIdU s
           -> UnifyM s ()
unifyInner cid1 insts1 uid1_u cid2 insts2 uid2_u = do
    when (cid1 /= cid2) $
        -- TODO: if we had a package identifier, could be an
        -- easier to understand error message.
        failWith $
            hang (text "Couldn't match component IDs:") 4
                 (text "   " <+> disp cid1 $$
                  text "and" <+> disp cid2)
    -- The KEY STEP which makes this a Huet-style unification
    -- algorithm.  (Also a payoff of using union-find.)
    -- We can build infinite unit IDs this way, which is necessary
    -- for support mutual recursion. NB: union keeps the SECOND
    -- descriptor, so we always arrange for a UnitIdThunkU to live
    -- there.
    liftST $ UnionFind.union uid1_u uid2_u
    F.sequenceA_ $ Map.intersectionWith unifyModule insts1 insts2

-- | Imperatively unify two modules.
unifyModule :: ModuleU s -> ModuleU s -> UnifyM s ()
unifyModule mod1_u mod2_u
    | mod1_u == mod2_u = return ()
    | otherwise = do
        mod1 <- liftST $ UnionFind.find mod1_u
        mod2 <- liftST $ UnionFind.find mod2_u
        case (mod1, mod2) of
            (ModuleVarU _, _) -> liftST $ UnionFind.union mod1_u mod2_u
            (_, ModuleVarU _) -> liftST $ UnionFind.union mod2_u mod1_u
            (ModuleU uid1 mod_name1, ModuleU uid2 mod_name2) -> do
                when (mod_name1 /= mod_name2) $
                    failWith $
                        hang (text "Cannot match module names") 4 $
                            text "   " <+> disp mod_name1 $$
                            text "and" <+> disp mod_name2
                -- NB: this is not actually necessary (because we'll
                -- detect loops eventually in 'unifyUnitId'), but it
                -- seems harmless enough
                liftST $ UnionFind.union mod1_u mod2_u
                unifyUnitId uid1 uid2

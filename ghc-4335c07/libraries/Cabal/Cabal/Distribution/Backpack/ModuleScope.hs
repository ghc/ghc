{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.ModuleScope (
    -- * Module scopes
    ModuleScope(..),
    ModuleProvides,
    ModuleRequires,
    ModuleSource(..),
    dispModuleSource,
    WithSource(..),
    unWithSource,
    getSource,
    ModuleWithSource,
    emptyModuleScope,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.ModuleName
import Distribution.Types.IncludeRenaming
import Distribution.Types.PackageName
import Distribution.Types.ComponentName

import Distribution.Backpack
import Distribution.Backpack.ModSubst
import Distribution.Text

import qualified Data.Map as Map
import Text.PrettyPrint


-----------------------------------------------------------------------
-- Module scopes

-- Why is ModuleProvides so complicated?  The basic problem is that
-- we want to support this:
--
--  package p where
--      include q (A)
--      include r (A)
--      module B where
--          import "q" A
--          import "r" A
--
-- Specifically, in Cabal today it is NOT an error have two modules in
-- scope with the same identifier.  So we need to preserve this for
-- Backpack.  The modification is that an ambiguous module name is
-- OK... as long as it is NOT used to fill a requirement!
--
-- So as a first try, we might try deferring unifying provisions that
-- are being glommed together, and check for equality after the fact.
-- But this doesn't work, because what if a multi-module provision
-- is used to fill a requirement?!  So you do the equality test
-- IMMEDIATELY before a requirement fill happens... or never at all.
--
-- Alternate strategy: go ahead and unify, and then if it is revealed
-- that some requirements got filled "out-of-thin-air", error.


-- | A 'ModuleScope' describes the modules and requirements that
-- are in-scope as we are processing a Cabal package.  Unlike
-- a 'ModuleShape', there may be multiple modules in scope at
-- the same 'ModuleName'; this is only an error if we attempt
-- to use those modules to fill a requirement.  A 'ModuleScope'
-- can influence the 'ModuleShape' via a reexport.
data ModuleScope = ModuleScope {
    modScopeProvides :: ModuleProvides,
    modScopeRequires :: ModuleRequires
    }

-- | An empty 'ModuleScope'.
emptyModuleScope :: ModuleScope
emptyModuleScope = ModuleScope Map.empty Map.empty

-- | Every 'Module' in scope at a 'ModuleName' is annotated with
-- the 'PackageName' it comes from.
type ModuleProvides = Map ModuleName [ModuleWithSource]
-- | INVARIANT: entries for ModuleName m, have msrc_module is OpenModuleVar m
type ModuleRequires = Map ModuleName [ModuleWithSource]
-- TODO: consider newtping the two types above.

-- | Description of where a module participating in mixin linking came
-- from.
data ModuleSource
    = FromMixins         PackageName ComponentName IncludeRenaming
    | FromBuildDepends   PackageName ComponentName
    | FromExposedModules ModuleName
    | FromOtherModules   ModuleName
    | FromSignatures     ModuleName
-- We don't have line numbers, but if we did, we'd want to record that
-- too

-- TODO: Deduplicate this with Distribution.Backpack.UnifyM.ci_msg
dispModuleSource :: ModuleSource -> Doc
dispModuleSource (FromMixins pn cn incls)
  = text "mixins:" <+> dispComponent pn cn <+> disp incls
dispModuleSource (FromBuildDepends pn cn)
  = text "build-depends:" <+> dispComponent pn cn
dispModuleSource (FromExposedModules m)
  = text "exposed-modules:" <+> disp m
dispModuleSource (FromOtherModules m)
  = text "other-modules:" <+> disp m
dispModuleSource (FromSignatures m)
  = text "signatures:" <+> disp m

-- Dependency
dispComponent :: PackageName -> ComponentName -> Doc
dispComponent pn cn =
    -- NB: This syntax isn't quite the source syntax, but it
    -- should be clear enough.  To do source syntax, we'd
    -- need to know what the package we're linking is.
    case cn of
        CLibName -> disp pn
        CSubLibName ucn -> disp pn <<>> colon <<>> disp ucn
        -- Case below shouldn't happen
        _ -> disp pn <+> parens (disp cn)

-- | An 'OpenModule', annotated with where it came from in a Cabal file.
data WithSource a = WithSource ModuleSource a
    deriving (Functor, Foldable, Traversable)
unWithSource :: WithSource a -> a
unWithSource (WithSource _ x) = x
getSource :: WithSource a -> ModuleSource
getSource (WithSource s _) = s
type ModuleWithSource = WithSource OpenModule

instance ModSubst a => ModSubst (WithSource a) where
    modSubst subst (WithSource s m) = WithSource s (modSubst subst m)

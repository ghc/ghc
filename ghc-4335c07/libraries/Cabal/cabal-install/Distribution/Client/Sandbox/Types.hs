-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox.Types
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Helpers for writing code that works both inside and outside a sandbox.
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox.Types (
  UseSandbox(..), isUseSandbox, whenUsingSandbox,
  SandboxPackageInfo(..)
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import Distribution.Client.Types (UnresolvedSourcePackage)

import qualified Data.Set as S

-- | Are we using a sandbox?
data UseSandbox = UseSandbox FilePath | NoSandbox

instance Monoid UseSandbox where
  mempty = NoSandbox
  mappend = (<>)

instance Semigroup UseSandbox where
  NoSandbox         <> s                 = s
  u0@(UseSandbox _) <> NoSandbox         = u0
  (UseSandbox _)    <> u1@(UseSandbox _) = u1

-- | Convert a @UseSandbox@ value to a boolean. Useful in conjunction with
-- @when@.
isUseSandbox :: UseSandbox -> Bool
isUseSandbox (UseSandbox _) = True
isUseSandbox NoSandbox      = False

-- | Execute an action only if we're in a sandbox, feeding to it the path to the
-- sandbox directory.
whenUsingSandbox :: UseSandbox -> (FilePath -> IO ()) -> IO ()
whenUsingSandbox NoSandbox               _   = return ()
whenUsingSandbox (UseSandbox sandboxDir) act = act sandboxDir

-- | Data about the packages installed in the sandbox that is passed from
-- 'reinstallAddSourceDeps' to the solver.
data SandboxPackageInfo = SandboxPackageInfo {
  modifiedAddSourceDependencies :: ![UnresolvedSourcePackage],
  -- ^ Modified add-source deps that we want to reinstall. These are guaranteed
  -- to be already installed in the sandbox.

  otherAddSourceDependencies    :: ![UnresolvedSourcePackage],
  -- ^ Remaining add-source deps. Some of these may be not installed in the
  -- sandbox.

  otherInstalledSandboxPackages :: !InstalledPackageIndex.InstalledPackageIndex,
  -- ^ All packages installed in the sandbox. Intersection with
  -- 'modifiedAddSourceDependencies' and/or 'otherAddSourceDependencies' can be
  -- non-empty.

  allAddSourceDependencies      :: !(S.Set FilePath)
  -- ^ A set of paths to all add-source dependencies, for convenience.
  }

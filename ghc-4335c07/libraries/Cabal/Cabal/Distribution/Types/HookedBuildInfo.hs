{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.HookedBuildInfo (
    HookedBuildInfo,
    emptyHookedBuildInfo,
  ) where

-- import Distribution.Compat.Prelude
import Distribution.Types.BuildInfo
import Distribution.Types.UnqualComponentName

-- | 'HookedBuildInfo' is mechanism that hooks can use to
-- override the 'BuildInfo's inside packages.  One example
-- use-case (which is used in core libraries today) is as
-- a way of passing flags which are computed by a configure
-- script into Cabal.  In this case, the autoconf build type adds
-- hooks to read in a textual 'HookedBuildInfo' format prior
-- to doing any operations.
--
-- Quite honestly, this mechanism is a massive hack since we shouldn't
-- be editing the 'PackageDescription' data structure (it's easy
-- to assume that this data structure shouldn't change and
-- run into bugs, see for example 1c20a6328579af9e37677d507e2e9836ef70ab9d).
-- But it's a bit convenient, because there isn't another data
-- structure that allows adding extra 'BuildInfo' style things.
--
-- In any case, a lot of care has to be taken to make sure the
-- 'HookedBuildInfo' is applied to the 'PackageDescription'.  In
-- general this process occurs in "Distribution.Simple", which is
-- responsible for orchestrating the hooks mechanism.  The
-- general strategy:
--
--      1. We run the pre-hook, which produces a 'HookedBuildInfo'
--         (e.g., in the Autoconf case, it reads it out from a file).
--      2. We sanity-check the hooked build info with
--         'sanityCheckHookedBuildInfo'.
--      3. We update our 'PackageDescription' (either freshly read
--         or cached from 'LocalBuildInfo') with 'updatePackageDescription'.
--
--         In principle, we are also supposed to update the copy of
--         the 'PackageDescription' stored in 'LocalBuildInfo'
--         at 'localPkgDescr'.  Unfortunately, in practice, there
--         are lots of Custom setup scripts which fail to update
--         'localPkgDescr' so you really shouldn't rely on it.
--         It's not DEPRECATED because there are legitimate uses
--         for it, but... yeah.  Sharp knife.  See
--         <https://github.com/haskell/cabal/issues/3606>
--         for more information on the issue.
--
-- It is not well-specified whether or not a 'HookedBuildInfo' applied
-- at configure time is persistent to the 'LocalBuildInfo'.  The
-- fact that 'HookedBuildInfo' is passed to 'confHook' MIGHT SUGGEST
-- that the 'HookedBuildInfo' is applied at this time, but actually
-- since 9317b67e6122ab14e53f81b573bd0ecb388eca5a it has been ONLY used
-- to create a modified package description that we check for problems:
-- it is never actually saved to the LBI.  Since 'HookedBuildInfo' is
-- applied monoidally to the existing build infos (and it is not an
-- idempotent monoid), it could break things to save it, since we
-- are obligated to apply any new 'HookedBuildInfo' and then we'd
-- get the effect twice.  But this does mean we have to re-apply
-- it every time. Hey, it's more flexibility.
type HookedBuildInfo = (Maybe BuildInfo, [(UnqualComponentName, BuildInfo)])

emptyHookedBuildInfo :: HookedBuildInfo
emptyHookedBuildInfo = (Nothing, [])

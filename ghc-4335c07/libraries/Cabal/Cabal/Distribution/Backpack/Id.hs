{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.Id(
    computeComponentId,
    computeCompatPackageKey,
    computeCompatPackageName,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.UnqualComponentName
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Simple.Setup as Setup
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.ComponentId
import Distribution.Types.PackageId
import Distribution.Types.UnitId
import Distribution.Types.MungedPackageName
import Distribution.Utils.Base62
import Distribution.Version

import Distribution.Text
    ( display, simpleParse )

-- | This method computes a default, "good enough" 'ComponentId'
-- for a package.  The intent is that cabal-install (or the user) will
-- specify a more detailed IPID via the @--ipid@ flag if necessary.
computeComponentId
    :: Bool -- deterministic mode
    -> Flag String
    -> Flag ComponentId
    -> PackageIdentifier
    -> ComponentName
    -- This is used by cabal-install's legacy codepath
    -> Maybe ([ComponentId], FlagAssignment)
    -> ComponentId
computeComponentId deterministic mb_ipid mb_cid pid cname mb_details =
    -- show is found to be faster than intercalate and then replacement of
    -- special character used in intercalating. We cannot simply hash by
    -- doubly concating list, as it just flatten out the nested list, so
    -- different sources can produce same hash
    let hash_suffix
            | Just (dep_ipids, flags) <- mb_details
            = "-" ++ hashToBase62
                -- For safety, include the package + version here
                -- for GHC 7.10, where just the hash is used as
                -- the package key
                    (    display pid
                      ++ show dep_ipids
                      ++ show flags     )
            | otherwise = ""
        generated_base = display pid ++ hash_suffix
        explicit_base cid0 = fromPathTemplate (InstallDirs.substPathTemplate env
                                                    (toPathTemplate cid0))
            -- Hack to reuse install dirs machinery
            -- NB: no real IPID available at this point
          where env = packageTemplateEnv pid (mkUnitId "")
        actual_base = case mb_ipid of
                        Flag ipid0 -> explicit_base ipid0
                        NoFlag | deterministic -> display pid
                               | otherwise     -> generated_base
    in case mb_cid of
          Flag cid -> cid
          NoFlag -> mkComponentId $ actual_base
                        ++ (case componentNameString cname of
                                Nothing -> ""
                                Just s -> "-" ++ unUnqualComponentName s)

-- | In GHC 8.0, the string we pass to GHC to use for symbol
-- names for a package can be an arbitrary, IPID-compatible string.
-- However, prior to GHC 8.0 there are some restrictions on what
-- format this string can be (due to how ghc-pkg parsed the key):
--
--      1. In GHC 7.10, the string had either be of the form
--      foo_ABCD, where foo is a non-semantic alphanumeric/hyphenated
--      prefix and ABCD is two base-64 encoded 64-bit integers,
--      or a GHC 7.8 style identifier.
--
--      2. In GHC 7.8, the string had to be a valid package identifier
--      like foo-0.1.
--
-- So, the problem is that Cabal, in general, has a general IPID,
-- but needs to figure out a package key / package ID that the
-- old ghc-pkg will actually accept.  But there's an EVERY WORSE
-- problem: if ghc-pkg decides to parse an identifier foo-0.1-xxx
-- as if it were a package identifier, which means it will SILENTLY
-- DROP the "xxx" (because it's a tag, and Cabal does not allow tags.)
-- So we must CONNIVE to ensure that we don't pick something that
-- looks like this.
--
-- So this function attempts to define a mapping into the old formats.
--
-- The mapping for GHC 7.8 and before:
--
--      * We use the *compatibility* package name and version.  For
--        public libraries this is just the package identifier; for
--        internal libraries, it's something like "z-pkgname-z-libname-0.1".
--        See 'computeCompatPackageName' for more details.
--
-- The mapping for GHC 7.10:
--
--      * For CLibName:
--          If the IPID is of the form foo-0.1-ABCDEF where foo_ABCDEF would
--          validly parse as a package key, we pass "ABCDEF".  (NB: not
--          all hashes parse this way, because GHC 7.10 mandated that
--          these hashes be two base-62 encoded 64 bit integers),
--          but hashes that Cabal generated using 'computeComponentId'
--          are guaranteed to have this form.
--
--          If it is not of this form, we rehash the IPID into the
--          correct form and pass that.
--
--      * For sub-components, we rehash the IPID into the correct format
--        and pass that.
--
computeCompatPackageKey
    :: Compiler
    -> MungedPackageName
    -> Version
    -> UnitId
    -> String
computeCompatPackageKey comp pkg_name pkg_version uid
    | not (packageKeySupported comp) =
        display pkg_name ++ "-" ++ display pkg_version
    | not (unifiedIPIDRequired comp) =
        let str = unUnitId uid -- assume no Backpack support
            mb_verbatim_key
                = case simpleParse str :: Maybe PackageId of
                    -- Something like 'foo-0.1', use it verbatim.
                    -- (NB: hash tags look like tags, so they are parsed,
                    -- so the extra equality check tests if a tag was dropped.)
                    Just pid0 | display pid0 == str -> Just str
                    _ -> Nothing
            mb_truncated_key
                = let cand = reverse (takeWhile isAlphaNum (reverse str))
                  in if length cand == 22 && all isAlphaNum cand
                        then Just cand
                        else Nothing
            rehashed_key = hashToBase62 str
        in fromMaybe rehashed_key (mb_verbatim_key `mplus` mb_truncated_key)
    | otherwise = display uid

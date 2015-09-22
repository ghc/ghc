{-# LANGUAGE CPP #-}
module ShPackageKey(
    ShFreeHoles,
    calcModuleFreeHoles,

    newPackageKey,
    newPackageKeyWithScope,
    lookupPackageKey,

    generalizeHoleModule,
    canonicalizeModule,

    pprPackageKey
) where

#include "HsVersions.h"

import Module
import Packages
import Encoding
import FastString
import UniqFM
import UniqSet
import Outputable
import Util
import DynFlags

import System.IO.Unsafe ( unsafePerformIO )
import Control.Monad
import Data.IORef
import GHC.Fingerprint
import Data.List
import Data.Function

-- NB: didn't put this in Module, that seems a bit too low in the
-- hierarchy, need to refer to DynFlags

{-
************************************************************************
*                                                                      *
                        Package Keys
*                                                                      *
************************************************************************
-}

-- Note: [PackageKey cache]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- The built-in PackageKey type (used by Module, Name, etc)
-- records the instantiation of the package as an MD5 hash
-- which is not reversible without some extra information.
-- However, the shape merging process requires us to be able
-- to substitute Module occurrences /inside/ the package key.
--
-- Thus, we maintain the invariant: for every PackageKey
-- in our system, either:
--
--      1. It is in the installed package database (lookupPackage)
--         so we can lookup the recorded instantiatedWith
--      2. We've recorded the associated mapping in the
--         PackageKeyCache.
--
-- A PackageKey can be expanded into a ShPackageKey which has
-- the instance mapping.  In the mapping, we don't bother
-- expanding a 'Module'; depending on 'shPackageKeyFreeHoles',
-- it may not be necessary to do a substitution (you only
-- need to drill down when substituing HOLE:H if H is in scope.

-- Note: [Module name in scope set]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Similar to InScopeSet, ShFreeHoles is an optimization that
-- allows us to avoid expanding a PackageKey into an ShPackageKey
-- if there isn't actually anything in the module expression that
-- we can substitute.

-- | Given a Name or Module, the 'ShFreeHoles' contains the set
-- of free variables, i.e. HOLE:A modules, which may be substituted.
-- If this set is empty no substitutions are possible.
type ShFreeHoles = UniqSet ModuleName

-- | Calculate the free holes of a 'Module'.
calcModuleFreeHoles :: DynFlags -> Module -> IO ShFreeHoles
calcModuleFreeHoles dflags m
    | modulePackageKey m == holePackageKey = return (unitUniqSet (moduleName m))
    | otherwise = do
        shpk <- lookupPackageKey dflags (modulePackageKey m)
        return $ case shpk of
            ShDefinitePackageKey{} -> emptyUniqSet
            ShPackageKey{ shPackageKeyFreeHoles = in_scope } -> in_scope

-- | Calculate the free holes of the hole map @[('ModuleName', 'Module')]@.
calcInstsFreeHoles :: DynFlags -> [(ModuleName, Module)] -> IO ShFreeHoles
calcInstsFreeHoles dflags insts =
    fmap unionManyUniqSets (mapM (calcModuleFreeHoles dflags . snd) insts)

-- | Given a 'UnitName', a 'LibraryName', and sorted mapping of holes to
-- their implementations, compute the 'PackageKey' associated with it, as well
-- as the recursively computed 'ShFreeHoles' of holes that may be substituted.
newPackageKeyWithScope :: DynFlags
                       -> UnitName
                       -> LibraryName
                       -> [(ModuleName, Module)]
                       -> IO (PackageKey, ShFreeHoles)
newPackageKeyWithScope dflags pn vh insts = do
    fhs <- calcInstsFreeHoles dflags insts
    pk <- newPackageKey' dflags (ShPackageKey pn vh insts fhs)
    return (pk, fhs)

-- | Given a 'UnitName' and sorted mapping of holes to
-- their implementations, compute the 'PackageKey' associated with it.
-- (Analogous to 'newGlobalBinder').
newPackageKey :: DynFlags
              -> UnitName
              -> LibraryName
              -> [(ModuleName, Module)]
              -> IO PackageKey
newPackageKey dflags pn vh insts = do
    (pk, _) <- newPackageKeyWithScope dflags pn vh insts
    return pk

-- | Given a 'ShPackageKey', compute the 'PackageKey' associated with it.
-- This function doesn't calculate the 'ShFreeHoles', because it is
-- provided with 'ShPackageKey'.
newPackageKey' :: DynFlags -> ShPackageKey -> IO PackageKey
newPackageKey' _ (ShDefinitePackageKey pk) = return pk
newPackageKey' dflags
               shpk@(ShPackageKey pn vh insts fhs) = do
    ASSERTM( fmap (==fhs) (calcInstsFreeHoles dflags insts) )
    let pk = mkPackageKey pn vh insts
        pkt_var = pkgKeyCache dflags
    pk_cache <- readIORef pkt_var
    let consistent pk_cache = maybe True (==shpk) (lookupUFM pk_cache pk)
    MASSERT( consistent pk_cache )
    when (not (elemUFM pk pk_cache)) $
        atomicModifyIORef' pkt_var (\pk_cache ->
            -- Could race, but it's guaranteed to be the same
            ASSERT( consistent pk_cache ) (addToUFM pk_cache pk shpk, ()))
    return pk

-- | Given a 'PackageKey', reverse lookup the 'ShPackageKey' associated
-- with it.  This only gives useful information for keys which are
-- created using 'newPackageKey' or the associated functions, or that are
-- already in the installed package database, since we generally cannot reverse
-- MD5 hashes.
lookupPackageKey :: DynFlags
                 -> PackageKey
                 -> IO ShPackageKey
lookupPackageKey dflags pk
  | pk `elem` wiredInPackageKeys
     || pk == mainPackageKey
     || pk == holePackageKey
  = return (ShDefinitePackageKey pk)
  | otherwise = do
    let pkt_var = pkgKeyCache dflags
    pk_cache <- readIORef pkt_var
    case lookupUFM pk_cache pk of
        Just r -> return r
        _ -> return (ShDefinitePackageKey pk)

pprPackageKey :: PackageKey -> SDoc
pprPackageKey pk = sdocWithDynFlags $ \dflags ->
    -- name cache is a memotable
    let shpk = unsafePerformIO (lookupPackageKey dflags pk)
    in case shpk of
        shpk@ShPackageKey{} ->
            ppr (shPackageKeyUnitName shpk) <>
                parens (hsep
                    (punctuate comma [ ppUnless (moduleName m == modname)
                                                (ppr modname <+> text "->")
                                       <+> ppr m
                                     | (modname, m) <- shPackageKeyInsts shpk]))
            <> ifPprDebug (braces (ftext (packageKeyFS pk)))
        ShDefinitePackageKey pk -> ftext (packageKeyFS pk)

-- NB: newPackageKey and lookupPackageKey are mutually recursive; this
-- recursion is guaranteed to bottom out because you can't set up cycles
-- of PackageKeys.


{-
************************************************************************
*                                                                      *
                        Package key hashing
*                                                                      *
************************************************************************
-}

-- | Generates a 'PackageKey'.  Don't call this directly; you probably
-- want to cache the result.
mkPackageKey :: UnitName
             -> LibraryName
             -> [(ModuleName, Module)] -- hole instantiations
             -> PackageKey
mkPackageKey (UnitName fsUnitName)
             (LibraryName fsLibraryName) unsorted_holes =
    -- NB: don't use concatFS here, it's not much of an improvement
    fingerprintPackageKey . fingerprintString $
        unpackFS fsUnitName ++ "\n" ++
        unpackFS fsLibraryName ++ "\n" ++
        concat [ moduleNameString m
                ++ " " ++ packageKeyString (modulePackageKey b)
                ++ ":" ++ moduleNameString (moduleName b) ++ "\n"
               | (m, b) <- sortBy (stableModuleNameCmp `on` fst) unsorted_holes]

-- | Generalize a 'Module' into one where all the holes are indefinite.
-- @p(A -> ...):C@ generalizes to @p(A -> HOLE:A):C@.  Useful when
-- you need to figure out if you've already type-checked the generalized
-- version of this module, so you don't have to do the whole rigamarole.
generalizeHoleModule :: DynFlags -> Module -> IO Module
generalizeHoleModule dflags m = do
    pk <- generalizeHolePackageKey dflags (modulePackageKey m)
    return (mkModule pk (moduleName m))

-- | Generalize a 'PackageKey' into one where all the holes are indefinite.
-- @p(A -> q():A) generalizes to p(A -> HOLE:A)@.
generalizeHolePackageKey :: DynFlags -> PackageKey -> IO PackageKey
generalizeHolePackageKey dflags pk = do
    shpk <- lookupPackageKey dflags pk
    case shpk of
        ShDefinitePackageKey _ -> return pk
        ShPackageKey { shPackageKeyUnitName = pn,
                       shPackageKeyLibraryName = vh,
                       shPackageKeyInsts = insts0 }
          -> let insts = map (\(x, _) -> (x, mkModule holePackageKey x)) insts0
             in newPackageKey dflags pn vh insts

-- | Canonicalize a 'Module' so that it uniquely identifies a module.
-- For example, @p(A -> M):A@ canonicalizes to @M@.  Useful for making
-- sure the interface you've loaded as the right @mi_module@.
canonicalizeModule :: DynFlags -> Module -> IO Module
canonicalizeModule dflags m = do
    let pk = modulePackageKey m
    shpk <- lookupPackageKey dflags pk
    return $ case shpk of
        ShPackageKey { shPackageKeyInsts = insts }
            | Just m' <- lookup (moduleName m) insts -> m'
        _ -> m

fingerprintPackageKey :: Fingerprint -> PackageKey
fingerprintPackageKey (Fingerprint a b)
    = stringToPackageKey (toBase62Padded a ++ toBase62Padded b)
      -- See Note [Base 62 encoding 128-bit integers]

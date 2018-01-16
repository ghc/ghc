{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module defines the core data types for Backpack.  For more
-- details, see:
--
--  <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>

module Distribution.Backpack (
    -- * OpenUnitId
    OpenUnitId(..),
    openUnitIdFreeHoles,
    mkOpenUnitId,

    -- * DefUnitId
    DefUnitId,
    unDefUnitId,
    mkDefUnitId,

    -- * OpenModule
    OpenModule(..),
    openModuleFreeHoles,

    -- * OpenModuleSubst
    OpenModuleSubst,
    dispOpenModuleSubst,
    dispOpenModuleSubstEntry,
    parseOpenModuleSubst,
    parseOpenModuleSubstEntry,
    openModuleSubstFreeHoles,

    -- * Conversions to 'UnitId'
    abstractUnitId,
    hashModuleSubst,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding (mod)
import Distribution.Compat.ReadP
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint (hcat)

import Distribution.ModuleName
import Distribution.Text
import Distribution.Types.ComponentId
import Distribution.Types.UnitId
import Distribution.Types.Module
import Distribution.Utils.Base62

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------
-- OpenUnitId

-- | An 'OpenUnitId' describes a (possibly partially) instantiated
-- Backpack component, with a description of how the holes are filled
-- in.  Unlike 'OpenUnitId', the 'ModuleSubst' is kept in a structured
-- form that allows for substitution (which fills in holes.) This form
-- of unit cannot be installed. It must first be converted to a
-- 'UnitId'.
--
-- In the absence of Backpack, there are no holes to fill, so any such
-- component always has an empty module substitution; thus we can lossly
-- represent it as an 'OpenUnitId uid'.
--
-- For a source component using Backpack, however, there is more
-- structure as components may be parametrized over some signatures, and
-- these \"holes\" may be partially or wholly filled.
--
-- OpenUnitId plays an important role when we are mix-in linking,
-- and is recorded to the installed packaged database for indefinite
-- packages; however, for compiled packages that are fully instantiated,
-- we instantiate 'OpenUnitId' into 'UnitId'.
--
-- For more details see the Backpack spec
-- <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
--

data OpenUnitId
    -- | Identifies a component which may have some unfilled holes;
    -- specifying its 'ComponentId' and its 'OpenModuleSubst'.
    -- TODO: Invariant that 'OpenModuleSubst' is non-empty?
    -- See also the Text instance.
    = IndefFullUnitId ComponentId OpenModuleSubst
    -- | Identifies a fully instantiated component, which has
    -- been compiled and abbreviated as a hash.  The embedded 'UnitId'
    -- MUST NOT be for an indefinite component; an 'OpenUnitId'
    -- is guaranteed not to have any holes.
    | DefiniteUnitId DefUnitId
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)
-- TODO: cache holes?

instance Binary OpenUnitId

instance NFData OpenUnitId where
    rnf (IndefFullUnitId cid subst) = rnf cid `seq` rnf subst
    rnf (DefiniteUnitId uid) = rnf uid

instance Text OpenUnitId where
    disp (IndefFullUnitId cid insts)
        -- TODO: arguably a smart constructor to enforce invariant would be
        -- better
        | Map.null insts = disp cid
        | otherwise      = disp cid <<>> Disp.brackets (dispOpenModuleSubst insts)
    disp (DefiniteUnitId uid) = disp uid
    parse = parseOpenUnitId <++ fmap DefiniteUnitId parse
      where
        parseOpenUnitId = do
            cid <- parse
            insts <- Parse.between (Parse.char '[') (Parse.char ']')
                       parseOpenModuleSubst
            return (IndefFullUnitId cid insts)

-- | Get the set of holes ('ModuleVar') embedded in a 'UnitId'.
openUnitIdFreeHoles :: OpenUnitId -> Set ModuleName
openUnitIdFreeHoles (IndefFullUnitId _ insts) = openModuleSubstFreeHoles insts
openUnitIdFreeHoles _ = Set.empty

-- | Safe constructor from a UnitId.  The only way to do this safely
-- is if the instantiation is provided.
mkOpenUnitId :: UnitId -> ComponentId -> OpenModuleSubst -> OpenUnitId
mkOpenUnitId uid cid insts =
    if Set.null (openModuleSubstFreeHoles insts)
        then DefiniteUnitId (unsafeMkDefUnitId uid) -- invariant holds!
        else IndefFullUnitId cid insts

-----------------------------------------------------------------------
-- DefUnitId

-- | Create a 'DefUnitId' from a 'ComponentId' and an instantiation
-- with no holes.
mkDefUnitId :: ComponentId -> Map ModuleName Module -> DefUnitId
mkDefUnitId cid insts =
    unsafeMkDefUnitId (mkUnitId
        (unComponentId cid ++ maybe "" ("+"++) (hashModuleSubst insts)))
        -- impose invariant!

-----------------------------------------------------------------------
-- OpenModule

-- | Unlike a 'Module', an 'OpenModule' is either an ordinary
-- module from some unit, OR an 'OpenModuleVar', representing a
-- hole that needs to be filled in.  Substitutions are over
-- module variables.
data OpenModule
    = OpenModule OpenUnitId ModuleName
    | OpenModuleVar ModuleName
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary OpenModule

instance NFData OpenModule where
    rnf (OpenModule uid mod_name) = rnf uid `seq` rnf mod_name
    rnf (OpenModuleVar mod_name) = rnf mod_name

instance Text OpenModule where
    disp (OpenModule uid mod_name) =
        hcat [disp uid, Disp.text ":", disp mod_name]
    disp (OpenModuleVar mod_name) =
        hcat [Disp.char '<', disp mod_name, Disp.char '>']
    parse = parseModuleVar <++ parseOpenModule
      where
        parseOpenModule = do
            uid <- parse
            _ <- Parse.char ':'
            mod_name <- parse
            return (OpenModule uid mod_name)
        parseModuleVar = do
            _ <- Parse.char '<'
            mod_name <- parse
            _ <- Parse.char '>'
            return (OpenModuleVar mod_name)

-- | Get the set of holes ('ModuleVar') embedded in a 'Module'.
openModuleFreeHoles :: OpenModule -> Set ModuleName
openModuleFreeHoles (OpenModuleVar mod_name) = Set.singleton mod_name
openModuleFreeHoles (OpenModule uid _n) = openUnitIdFreeHoles uid

-----------------------------------------------------------------------
-- OpenModuleSubst

-- | An explicit substitution on modules.
--
-- NB: These substitutions are NOT idempotent, for example, a
-- valid substitution is (A -> B, B -> A).
type OpenModuleSubst = Map ModuleName OpenModule

-- | Pretty-print the entries of a module substitution, suitable
-- for embedding into a 'OpenUnitId' or passing to GHC via @--instantiate-with@.
dispOpenModuleSubst :: OpenModuleSubst -> Disp.Doc
dispOpenModuleSubst subst
    = Disp.hcat
    . Disp.punctuate Disp.comma
    $ map dispOpenModuleSubstEntry (Map.toAscList subst)

-- | Pretty-print a single entry of a module substitution.
dispOpenModuleSubstEntry :: (ModuleName, OpenModule) -> Disp.Doc
dispOpenModuleSubstEntry (k, v) = disp k <<>> Disp.char '=' <<>> disp v

-- | Inverse to 'dispModSubst'.
parseOpenModuleSubst :: ReadP r OpenModuleSubst
parseOpenModuleSubst = fmap Map.fromList
      . flip Parse.sepBy (Parse.char ',')
      $ parseOpenModuleSubstEntry

-- | Inverse to 'dispModSubstEntry'.
parseOpenModuleSubstEntry :: ReadP r (ModuleName, OpenModule)
parseOpenModuleSubstEntry =
    do k <- parse
       _ <- Parse.char '='
       v <- parse
       return (k, v)

-- | Get the set of holes ('ModuleVar') embedded in a 'OpenModuleSubst'.
-- This is NOT the domain of the substitution.
openModuleSubstFreeHoles :: OpenModuleSubst -> Set ModuleName
openModuleSubstFreeHoles insts = Set.unions (map openModuleFreeHoles (Map.elems insts))

-----------------------------------------------------------------------
-- Conversions to UnitId

-- | When typechecking, we don't demand that a freshly instantiated
-- 'IndefFullUnitId' be compiled; instead, we just depend on the
-- installed indefinite unit installed at the 'ComponentId'.
abstractUnitId :: OpenUnitId -> UnitId
abstractUnitId (DefiniteUnitId def_uid) = unDefUnitId def_uid
abstractUnitId (IndefFullUnitId cid _) = newSimpleUnitId cid

-- | Take a module substitution and hash it into a string suitable for
-- 'UnitId'.  Note that since this takes 'Module', not 'OpenModule',
-- you are responsible for recursively converting 'OpenModule'
-- into 'Module'.  See also "Distribution.Backpack.ReadyComponent".
hashModuleSubst :: Map ModuleName Module -> Maybe String
hashModuleSubst subst
  | Map.null subst = Nothing
  | otherwise =
      Just . hashToBase62 $
        concat [ display mod_name ++ "=" ++ display m ++ "\n"
               | (mod_name, m) <- Map.toList subst]

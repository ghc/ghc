{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[Name]{@Name@: to transmit name info from renamer to typechecker}
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'OccName.OccName': see "OccName#name_types"
--
-- * 'RdrName.RdrName': see "RdrName#name_types"
--
-- *  'Name.Name' is the type of names that have had their scoping and binding resolved. They
--   have an 'OccName.OccName' but also a 'Unique.Unique' that disambiguates Names that have
--   the same 'OccName.OccName' and indeed is used for all 'Name.Name' comparison. Names
--   also contain information about where they originated from, see "Name#name_sorts"
--
-- * 'Id.Id': see "Id#name_types"
--
-- * 'Var.Var': see "Var#name_types"
--
-- #name_sorts#
-- Names are one of:
--
--  * External, if they name things declared in other modules. Some external
--    Names are wired in, i.e. they name primitives defined in the compiler itself
--
--  * Internal, if they name things in the module being compiled. Some internal
--    Names are system names, if they are names manufactured by the compiler

module Name (
        -- * The main types
        Name,                                   -- Abstract
        BuiltInSyntax(..),

        -- ** Creating 'Name's
        mkSystemName, mkSystemNameAt,
        mkInternalName, mkClonedInternalName, mkDerivedInternalName,
        mkSystemVarName, mkSysTvName,
        mkFCallName,
        mkExternalName, mkWiredInName,

        -- ** Manipulating and deconstructing 'Name's
        nameUnique, setNameUnique,
        nameOccName, nameModule_maybe,
        setNameLoc,
        tidyNameOcc,
        localiseName,

        nameSrcLoc, nameSrcSpan,

        -- ** Predicates on 'Name's
        isSystemName, isInternalName, isExternalName,
        isTyVarName, isTyConName, isDataConName,
        isValName, isVarName,
        isWiredInName, isBuiltInSyntax,
        wiredInNameTyThing_maybe,
        nameIsLocalOrFrom, nameIsHomePackage,
        nameIsHomePackageImport, nameIsFromExternalPackage,
        stableNameCmp,

        -- * Class 'NamedThing' and overloaded friends
        NamedThing(..),
        getSrcLoc, getSrcSpan, getOccString, getOccFS,

        nameStableString,

        -- Re-export the OccName stuff
        module OccName
    ) where

import GhcPrelude

import {-# SOURCE #-} TyCoRep( TyThing )

import OccName
import Module
import Name.Internal
import SrcLoc
import Unique
import Util
import Maybes
import Binary
import FastString
import Outputable

import Control.DeepSeq
import Data.Data

{-
************************************************************************
*                                                                      *
\subsection{Predicates on names}
*                                                                      *
************************************************************************
-}

isInternalName    :: Name -> Bool
isExternalName    :: Name -> Bool
isSystemName      :: Name -> Bool
isWiredInName     :: Name -> Bool

isWiredInName (Name {n_sort = WiredIn _ _ _}) = True
isWiredInName _                               = False

wiredInNameTyThing_maybe :: Name -> Maybe TyThing
wiredInNameTyThing_maybe (Name {n_sort = WiredIn _ thing _}) = Just thing
wiredInNameTyThing_maybe _                                   = Nothing

isBuiltInSyntax :: Name -> Bool
isBuiltInSyntax (Name {n_sort = WiredIn _ _ BuiltInSyntax}) = True
isBuiltInSyntax _                                           = False

isExternalName (Name {n_sort = External _})    = True
isExternalName (Name {n_sort = WiredIn _ _ _}) = True
isExternalName _                               = False

isInternalName name = not (isExternalName name)

nameModule_maybe :: Name -> Maybe Module
nameModule_maybe (Name { n_sort = External mod})    = Just mod
nameModule_maybe (Name { n_sort = WiredIn mod _ _}) = Just mod
nameModule_maybe _                                  = Nothing

nameIsLocalOrFrom :: Module -> Name -> Bool
-- ^ Returns True if the name is
--   (a) Internal
--   (b) External but from the specified module
--   (c) External but from the 'interactive' package
--
-- The key idea is that
--    False means: the entity is defined in some other module
--                 you can find the details (type, fixity, instances)
--                     in some interface file
--                 those details will be stored in the EPT or HPT
--
--    True means:  the entity is defined in this module or earlier in
--                     the GHCi session
--                 you can find details (type, fixity, instances) in the
--                     TcGblEnv or TcLclEnv
--
-- The isInteractiveModule part is because successive interactions of a GHCi session
-- each give rise to a fresh module (Ghci1, Ghci2, etc), but they all come
-- from the magic 'interactive' package; and all the details are kept in the
-- TcLclEnv, TcGblEnv, NOT in the HPT or EPT.
-- See Note [The interactive package] in HscTypes

nameIsLocalOrFrom from name
  | Just mod <- nameModule_maybe name = from == mod || isInteractiveModule mod
  | otherwise                         = True

nameIsHomePackage :: Module -> Name -> Bool
-- True if the Name is defined in module of this package
nameIsHomePackage this_mod
  = \nm -> case n_sort nm of
              External nm_mod    -> moduleUnitId nm_mod == this_pkg
              WiredIn nm_mod _ _ -> moduleUnitId nm_mod == this_pkg
              Internal -> True
              System   -> False
  where
    this_pkg = moduleUnitId this_mod

nameIsHomePackageImport :: Module -> Name -> Bool
-- True if the Name is defined in module of this package
-- /other than/ the this_mod
nameIsHomePackageImport this_mod
  = \nm -> case nameModule_maybe nm of
              Nothing -> False
              Just nm_mod -> nm_mod /= this_mod
                          && moduleUnitId nm_mod == this_pkg
  where
    this_pkg = moduleUnitId this_mod

-- | Returns True if the Name comes from some other package: neither this
-- package nor the interactive package.
nameIsFromExternalPackage :: UnitId -> Name -> Bool
nameIsFromExternalPackage this_pkg name
  | Just mod <- nameModule_maybe name
  , moduleUnitId mod /= this_pkg    -- Not this package
  , not (isInteractiveModule mod)       -- Not the 'interactive' package
  = True
  | otherwise
  = False

isTyVarName :: Name -> Bool
isTyVarName name = isTvOcc (nameOccName name)

isTyConName :: Name -> Bool
isTyConName name = isTcOcc (nameOccName name)

isDataConName :: Name -> Bool
isDataConName name = isDataOcc (nameOccName name)

isValName :: Name -> Bool
isValName name = isValOcc (nameOccName name)

isVarName :: Name -> Bool
isVarName = isVarOcc . nameOccName

isSystemName (Name {n_sort = System}) = True
isSystemName _                        = False

{-
************************************************************************
*                                                                      *
\subsection{Making names}
*                                                                      *
************************************************************************
-}

-- | Create a name which is (for now at least) local to the current module and hence
-- does not need a 'Module' to disambiguate it from other 'Name's
mkInternalName :: Unique -> OccName -> SrcSpan -> Name
mkInternalName uniq occ loc = Name { n_uniq = uniq
                                   , n_sort = Internal
                                   , n_occ = occ
                                   , n_loc = loc }
        -- NB: You might worry that after lots of huffing and
        -- puffing we might end up with two local names with distinct
        -- uniques, but the same OccName.  Indeed we can, but that's ok
        --      * the insides of the compiler don't care: they use the Unique
        --      * when printing for -ddump-xxx you can switch on -dppr-debug to get the
        --        uniques if you get confused
        --      * for interface files we tidyCore first, which makes
        --        the OccNames distinct when they need to be

mkClonedInternalName :: Unique -> Name -> Name
mkClonedInternalName uniq (Name { n_occ = occ, n_loc = loc })
  = Name { n_uniq = uniq, n_sort = Internal
         , n_occ = occ, n_loc = loc }

mkDerivedInternalName :: (OccName -> OccName) -> Unique -> Name -> Name
mkDerivedInternalName derive_occ uniq (Name { n_occ = occ, n_loc = loc })
  = Name { n_uniq = uniq, n_sort = Internal
         , n_occ = derive_occ occ, n_loc = loc }

-- | Create a name which definitely originates in the given module
mkExternalName :: Unique -> Module -> OccName -> SrcSpan -> Name
-- WATCH OUT! External Names should be in the Name Cache
-- (see Note [The Name Cache] in IfaceEnv), so don't just call mkExternalName
-- with some fresh unique without populating the Name Cache
mkExternalName uniq mod occ loc
  = Name { n_uniq = uniq, n_sort = External mod,
           n_occ = occ, n_loc = loc }

-- | Create a name which is actually defined by the compiler itself
mkWiredInName :: Module -> OccName -> Unique -> TyThing -> BuiltInSyntax -> Name
mkWiredInName mod occ uniq thing built_in
  = Name { n_uniq = uniq,
           n_sort = WiredIn mod thing built_in,
           n_occ = occ, n_loc = wiredInSrcSpan }

-- | Create a name brought into being by the compiler
mkSystemName :: Unique -> OccName -> Name
mkSystemName uniq occ = mkSystemNameAt uniq occ noSrcSpan

mkSystemNameAt :: Unique -> OccName -> SrcSpan -> Name
mkSystemNameAt uniq occ loc = Name { n_uniq = uniq, n_sort = System
                                   , n_occ = occ, n_loc = loc }

mkSystemVarName :: Unique -> FastString -> Name
mkSystemVarName uniq fs = mkSystemName uniq (mkVarOccFS fs)

mkSysTvName :: Unique -> FastString -> Name
mkSysTvName uniq fs = mkSystemName uniq (mkTyVarOccFS fs)

-- | Make a name for a foreign call
mkFCallName :: Unique -> String -> Name
mkFCallName uniq str = mkInternalName uniq (mkVarOcc str) noSrcSpan
   -- The encoded string completely describes the ccall

-- When we renumber/rename things, we need to be
-- able to change a Name's Unique to match the cached
-- one in the thing it's the name of.  If you know what I mean.
setNameUnique :: Name -> Unique -> Name
setNameUnique name uniq = name {n_uniq = uniq}

-- This is used for hsigs: we want to use the name of the originally exported
-- entity, but edit the location to refer to the reexport site
setNameLoc :: Name -> SrcSpan -> Name
setNameLoc name loc = name {n_loc = loc}

tidyNameOcc :: Name -> OccName -> Name
-- We set the OccName of a Name when tidying
-- In doing so, we change System --> Internal, so that when we print
-- it we don't get the unique by default.  It's tidy now!
tidyNameOcc name@(Name { n_sort = System }) occ = name { n_occ = occ, n_sort = Internal}
tidyNameOcc name                            occ = name { n_occ = occ }

-- | Make the 'Name' into an internal name, regardless of what it was to begin with
localiseName :: Name -> Name
localiseName n = n { n_sort = Internal }

{-
************************************************************************
*                                                                      *
\subsection{Hashing and comparison}
*                                                                      *
************************************************************************
-}

cmpName :: Name -> Name -> Ordering
cmpName n1 n2 = n_uniq n1 `nonDetCmpUnique` n_uniq n2

-- | Compare Names lexicographically
-- This only works for Names that originate in the source code or have been
-- tidied.
stableNameCmp :: Name -> Name -> Ordering
stableNameCmp (Name { n_sort = s1, n_occ = occ1 })
              (Name { n_sort = s2, n_occ = occ2 })
  = (s1 `sort_cmp` s2) `thenCmp` (occ1 `compare` occ2)
    -- The ordinary compare on OccNames is lexicographic
  where
    -- Later constructors are bigger
    sort_cmp (External m1) (External m2)       = m1 `stableModuleCmp` m2
    sort_cmp (External {}) _                   = LT
    sort_cmp (WiredIn {}) (External {})        = GT
    sort_cmp (WiredIn m1 _ _) (WiredIn m2 _ _) = m1 `stableModuleCmp` m2
    sort_cmp (WiredIn {})     _                = LT
    sort_cmp Internal         (External {})    = GT
    sort_cmp Internal         (WiredIn {})     = GT
    sort_cmp Internal         Internal         = EQ
    sort_cmp Internal         System           = LT
    sort_cmp System           System           = EQ
    sort_cmp System           _                = GT

{-
************************************************************************
*                                                                      *
\subsection[Name-instances]{Instance declarations}
*                                                                      *
************************************************************************
-}

-- | The same comments as for `Name`'s `Ord` instance apply.
instance Eq Name where
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True }

-- | __Caution__: This instance is implemented via `nonDetCmpUnique`, which
-- means that the ordering is not stable across deserialization or rebuilds.
--
-- See `nonDetCmpUnique` for further information, and trac #15240 for a bug
-- caused by improper use of this instance.

-- For a deterministic lexicographic ordering, use `stableNameCmp`.
instance Ord Name where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = cmpName a b

instance Uniquable Name where
    getUnique = nameUnique

instance NamedThing Name where
    getName n = n

instance Data Name where
  -- don't traverse?
  toConstr _   = abstractConstr "Name"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Name"

{-
************************************************************************
*                                                                      *
\subsection{Binary}
*                                                                      *
************************************************************************
-}

-- | Assumes that the 'Name' is a non-binding one. See
-- 'IfaceSyn.putIfaceTopBndr' and 'IfaceSyn.getIfaceTopBndr' for serializing
-- binding 'Name's. See 'UserData' for the rationale for this distinction.
instance Binary Name where
   put_ bh name =
      case getUserData bh of
        UserData{ ud_put_nonbinding_name = put_name } -> put_name bh name

   get bh =
      case getUserData bh of
        UserData { ud_get_name = get_name } -> get_name bh


-- | Get a string representation of a 'Name' that's unique and stable
-- across recompilations. Used for deterministic generation of binds for
-- derived instances.
-- eg. "$aeson_70dylHtv1FFGeai1IoxcQr$Data.Aeson.Types.Internal$String"
nameStableString :: Name -> String
nameStableString Name{..} =
  nameSortStableString n_sort ++ "$" ++ occNameString n_occ

nameSortStableString :: NameSort -> String
nameSortStableString System = "$_sys"
nameSortStableString Internal = "$_in"
nameSortStableString (External mod) = moduleStableString mod
nameSortStableString (WiredIn mod _ _) = moduleStableString mod

{-
************************************************************************
*                                                                      *
\subsection{Overloaded functions related to Names}
*                                                                      *
************************************************************************
-}

-- | A class allowing convenient access to the 'Name' of various datatypes
class NamedThing a where
    getOccName :: a -> OccName
    getName    :: a -> Name

    getOccName n = nameOccName (getName n)      -- Default method

instance NamedThing e => NamedThing (Located e) where
    getName = getName . unLoc

getSrcLoc           :: NamedThing a => a -> SrcLoc
getSrcSpan          :: NamedThing a => a -> SrcSpan
getOccString        :: NamedThing a => a -> String
getOccFS            :: NamedThing a => a -> FastString

getSrcLoc           = nameSrcLoc           . getName
getSrcSpan          = nameSrcSpan          . getName
getOccString        = occNameString        . getOccName
getOccFS            = occNameFS            . getOccName

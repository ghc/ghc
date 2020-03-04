{-# OPTIONS_GHC -fno-warn-duplicate-exports -fno-warn-orphans #-}

-- | This module is not used by GHC itself.  Rather, it exports all of
-- the functions and types you are likely to need when writing a
-- plugin for GHC. So authors of plugins can probably get away simply
-- with saying "import GHC.Plugins".
--
-- Particularly interesting modules for plugin writers include
-- "GHC.Core" and "CoreMonad".
module GHC.Plugins(
        module GHC.Driver.Plugins,
        module RdrName, module OccName, module Name, module Var, module Id, module IdInfo,
        module CoreMonad, module GHC.Core, module Literal, module GHC.Core.DataCon,
        module GHC.Core.Utils, module GHC.Core.Make, module GHC.Core.FVs,
        module GHC.Core.Subst, module GHC.Core.Rules, module Annotations,
        module GHC.Driver.Session, module GHC.Driver.Packages,
        module Module, module GHC.Core.Type, module GHC.Core.TyCon, module GHC.Core.Coercion,
        module TysWiredIn, module GHC.Driver.Types, module BasicTypes,
        module VarSet, module VarEnv, module NameSet, module NameEnv,
        module UniqSet, module UniqFM, module FiniteMap,
        module Util, module GHC.Serialized, module SrcLoc, module Outputable,
        module UniqSupply, module Unique, module FastString,

        -- * Getting 'Name's
        thNameToGhcName
    ) where

-- Plugin stuff itself
import GHC.Driver.Plugins

-- Variable naming
import RdrName
import OccName  hiding  ( varName {- conflicts with Var.varName -} )
import Name     hiding  ( varName {- reexport from OccName, conflicts with Var.varName -} )
import Var
import Id       hiding  ( lazySetIdInfo, setIdExported, setIdNotExported {- all three conflict with Var -} )
import IdInfo

-- Core
import CoreMonad
import GHC.Core
import Literal
import GHC.Core.DataCon
import GHC.Core.Utils
import GHC.Core.Make
import GHC.Core.FVs
import GHC.Core.Subst hiding( substTyVarBndr, substCoVarBndr, extendCvSubst )
       -- These names are also exported by Type

-- Core "extras"
import GHC.Core.Rules
import Annotations

-- Pipeline-related stuff
import GHC.Driver.Session
import GHC.Driver.Packages

-- Important GHC types
import Module
import GHC.Core.Type hiding {- conflict with GHC.Core.Subst -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import GHC.Core.Coercion hiding {- conflict with GHC.Core.Subst -}
                ( substCo )
import GHC.Core.TyCon
import TysWiredIn
import GHC.Driver.Types
import BasicTypes hiding ( Version {- conflicts with Packages.Version -} )

-- Collections and maps
import VarSet
import VarEnv
import NameSet
import NameEnv
import UniqSet
import UniqFM
-- Conflicts with UniqFM:
--import LazyUniqFM
import FiniteMap

-- Common utilities
import Util
import GHC.Serialized
import SrcLoc
import Outputable
import UniqSupply
import Unique           ( Unique, Uniquable(..) )
import FastString
import Data.Maybe

import GHC.Iface.Env    ( lookupOrigIO )
import GhcPrelude
import MonadUtils       ( mapMaybeM )
import GHC.ThToHs       ( thRdrNameGuesses )
import TcEnv            ( lookupGlobal )

import qualified Language.Haskell.TH as TH

{- This instance is defined outside CoreMonad.hs so that
   CoreMonad does not depend on TcEnv -}
instance MonadThings CoreM where
    lookupThing name = do { hsc_env <- getHscEnv
                          ; liftIO $ lookupGlobal hsc_env name }

{-
************************************************************************
*                                                                      *
               Template Haskell interoperability
*                                                                      *
************************************************************************
-}

-- | Attempt to convert a Template Haskell name to one that GHC can
-- understand. Original TH names such as those you get when you use
-- the @'foo@ syntax will be translated to their equivalent GHC name
-- exactly. Qualified or unqualified TH names will be dynamically bound
-- to names in the module being compiled, if possible. Exact TH names
-- will be bound to the name they represent, exactly.
thNameToGhcName :: TH.Name -> CoreM (Maybe Name)
thNameToGhcName th_name
  =  do { names <- mapMaybeM lookup (thRdrNameGuesses th_name)
          -- Pick the first that works
          -- E.g. reify (mkName "A") will pick the class A in preference
          -- to the data constructor A
        ; return (listToMaybe names) }
  where
    lookup rdr_name
      | Just n <- isExact_maybe rdr_name   -- This happens in derived code
      = return $ if isExternalName n then Just n else Nothing
      | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
      = do { hsc_env <- getHscEnv
           ; Just <$> liftIO (lookupOrigIO hsc_env rdr_mod rdr_occ) }
      | otherwise = return Nothing

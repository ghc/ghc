{-# OPTIONS_GHC -fno-warn-duplicate-exports -fno-warn-orphans #-}

-- | This module is not used by GHC itself.  Rather, it exports all of
-- the functions and types you are likely to need when writing a
-- plugin for GHC. So authors of plugins can probably get away simply
-- with saying "import GhcPlugins".
--
-- Particularly interesting modules for plugin writers include
-- "CoreSyn" and "CoreMonad".
module GhcPlugins(
        module Plugins,
        module RdrName, module OccName, module Name, module Var, module Id, module IdInfo,
        module CoreMonad, module CoreSyn, module Literal, module DataCon,
        module CoreUtils, module MkCore, module CoreFVs, module CoreSubst,
        module Rules, module Annotations,
        module DynFlags, module Packages,
        module Module, module Type, module TyCon, module Coercion,
        module TysWiredIn, module HscTypes, module BasicTypes,
        module VarSet, module VarEnv, module NameSet, module NameEnv,
        module UniqSet, module UniqFM, module FiniteMap,
        module Util, module GHC.Serialized, module SrcLoc, module Outputable,
        module UniqSupply, module Unique, module FastString,

        -- * Getting 'Name's
        thNameToGhcName
    ) where

-- Plugin stuff itself
import Plugins

-- Variable naming
import RdrName
import OccName  hiding  ( varName {- conflicts with Var.varName -} )
import Name     hiding  ( varName {- reexport from OccName, conflicts with Var.varName -} )
import Var
import Id       hiding  ( lazySetIdInfo, setIdExported, setIdNotExported {- all three conflict with Var -} )
import IdInfo

-- Core
import CoreMonad
import CoreSyn
import Literal
import DataCon
import CoreUtils
import MkCore
import CoreFVs
import CoreSubst hiding( substTyVarBndr, substCoVarBndr, extendCvSubst )
       -- These names are also exported by Type

-- Core "extras"
import Rules
import Annotations

-- Pipeline-related stuff
import DynFlags
import Packages

-- Important GHC types
import Module
import Type     hiding {- conflict with CoreSubst -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import Coercion hiding {- conflict with CoreSubst -}
                ( substCo )
import TyCon
import TysWiredIn
import HscTypes
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

import IfaceEnv         ( lookupOrigIO )
import GhcPrelude
import MonadUtils       ( mapMaybeM )
import Convert          ( thRdrNameGuesses )
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

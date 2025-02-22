{-# OPTIONS_GHC -fno-warn-duplicate-exports -fno-warn-orphans #-}

-- | This module is not used by GHC itself.  Rather, it exports all of
-- the functions and types you are likely to need when writing a
-- plugin for GHC. So authors of plugins can probably get away simply
-- with saying "import GHC.Plugins".
--
-- Particularly interesting modules for plugin writers include
-- "GHC.Core" and "GHC.Core.Opt.Monad".
module GHC.Plugins
   ( module GHC.Driver.Plugins
   , module GHC.Types.Name.Reader
   , module GHC.Types.Name.Occurrence
   , module GHC.Types.Name
   , module GHC.Types.Var
   , module GHC.Types.Id
   , module GHC.Types.Id.Info
   , module GHC.Types.PkgQual
   , module GHC.Core.Opt.Monad
   , module GHC.Core.Opt.Pipeline.Types
   , module GHC.Core.Opt.Stats
   , module GHC.Core
   , module GHC.Types.Literal
   , module GHC.Core.DataCon
   , module GHC.Core.Utils
   , module GHC.Core.Make
   , module GHC.Core.FVs
   , module GHC.Core.Subst
   , module GHC.Core.Rules
   , module GHC.Types.Annotations
   , module GHC.Driver.Session
   , module GHC.Driver.Ppr
   , module GHC.Unit.State
   , module GHC.Unit.Module
   , module GHC.Unit.Home
   , module GHC.Core.Type
   , module GHC.Core.TyCon
   , module GHC.Core.Coercion
   , module GHC.Builtin.Types
   , module GHC.Driver.Env
   , module GHC.Types.Basic
   , module GHC.Types.Var.Set
   , module GHC.Types.Var.Env
   , module GHC.Types.Name.Set
   , module GHC.Types.Name.Env
   , module GHC.Types.Unique
   , module GHC.Types.Unique.Set
   , module GHC.Types.Unique.FM
   , module GHC.Data.FiniteMap
   , module GHC.Utils.Misc
   , module GHC.Serialized
   , module GHC.Types.SrcLoc
   , module GHC.Utils.Outputable
   , module GHC.Utils.Panic
   , module GHC.Types.Unique.Supply
   , module GHC.Data.FastString
   , module GHC.Tc.Errors.Hole.FitTypes   -- for hole-fit plugins
   , module GHC.Tc.Errors.Hole.Plugin   -- for hole-fit plugins
   , module GHC.Unit.Module.ModGuts
   , module GHC.Unit.Module.ModSummary
   , module GHC.Unit.Module.ModIface
   , module GHC.Types.Meta
   , module GHC.Types.SourceError
   , module GHC.Parser.Errors.Types
   , module GHC.Types.Error
   , module GHC.Hs
   , -- * Getting 'Name's
     thNameToGhcName
   , thNameToGhcNameIO
   )
where

-- Plugin stuff itself
import GHC.Driver.Plugins

-- Variable naming
import GHC.Types.TyThing
import GHC.Types.PkgQual
import GHC.Types.SourceError
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence  hiding  ( varName {- conflicts with Var.varName -} )
import GHC.Types.Name     hiding  ( varName {- reexport from OccName, conflicts with Var.varName -} )
import GHC.Types.Var
import GHC.Types.Id       hiding  ( lazySetIdInfo, setIdExported, setIdNotExported {- all three conflict with Var -} )
import GHC.Types.Id.Info

-- Core
import GHC.Core.Opt.Monad
import GHC.Core.Opt.Pipeline.Types
import GHC.Core.Opt.Stats
import GHC.Core
import GHC.Types.Literal
import GHC.Core.DataCon
import GHC.Core.Utils
import GHC.Core.Make
import GHC.Core.FVs
import GHC.Core.Subst hiding( substTyVarBndr, substCoVarBndr, extendCvSubst, extendSubstInScopeSet )
       -- These names are also exported by Type

import GHC.Core.Rules
import GHC.Types.Annotations
import GHC.Types.Meta

import GHC.Driver.Session
import GHC.Unit.State

import GHC.Unit.Home
import GHC.Unit.Module
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Core.Type hiding {- conflict with GHC.Core.Subst -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import GHC.Core.Coercion hiding {- conflict with GHC.Core.Subst -}
                ( substCo )
import GHC.Core.TyCon
import GHC.Builtin.Types
import GHC.Driver.Env
import GHC.Types.Basic

-- Collections and maps
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
-- Conflicts with UniqFM:
--import LazyUniqFM
import GHC.Data.FiniteMap

-- Common utilities
import GHC.Utils.Misc
import GHC.Serialized
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Driver.Ppr
import GHC.Types.Unique.Supply
import GHC.Types.Unique ( Unique, Uniquable(..) )
import GHC.Data.FastString
import Data.Maybe

import GHC.Iface.Env    ( lookupNameCache )
import GHC.Prelude
import GHC.Utils.Monad  ( mapMaybeM )
import GHC.ThToHs       ( thRdrNameGuesses )
import GHC.Tc.Utils.Env ( lookupGlobal )
import GHC.Types.Name.Cache ( NameCache )

import GHC.Tc.Errors.Hole.FitTypes
import GHC.Tc.Errors.Hole.Plugin

-- For parse result plugins
import GHC.Parser.Errors.Types ( PsWarning, PsError )
import GHC.Types.Error         ( Messages )
import GHC.Hs                  ( HsParsedModule )

import qualified GHC.Boot.TH.Syntax as TH

{- This instance is defined outside GHC.Core.Opt.Monad so that
   GHC.Core.Opt.Monad does not depend on GHC.Tc.Utils.Env -}
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
thNameToGhcName th_name = do
  hsc_env <- getHscEnv
  liftIO $ thNameToGhcNameIO (hsc_NC hsc_env) th_name

-- | Attempt to convert a Template Haskell name to one that GHC can
-- understand. Original TH names such as those you get when you use
-- the @'foo@ syntax will be translated to their equivalent GHC name
-- exactly. Qualified or unqualified TH names will be dynamically bound
-- to names in the module being compiled, if possible. Exact TH names
-- will be bound to the name they represent, exactly.
--
-- One must be careful to consistently use the same 'NameCache' to
-- create identifier that might be compared. (C.f. how the
-- 'Control.Monad.ST.ST' Monad enforces that variables from separate
-- 'Control.Monad.ST.runST' invocations are never intermingled; it would
-- be valid to use the same tricks for 'Name's and 'NameCache's.)
--
-- For now, the easiest and recommended way to ensure a consistent
-- 'NameCache' is used it to retrieve the preexisting one from an active
-- 'HscEnv'. A single 'HscEnv' is created per GHC "session", and this
-- ensures everything in that session will get the same name cache.
thNameToGhcNameIO :: NameCache -> TH.Name -> IO (Maybe Name)
thNameToGhcNameIO cache th_name
  =  do { let listTuplePuns = True -- conservative assumption
        ; names <- mapMaybeM lookup (thRdrNameGuesses listTuplePuns th_name)
          -- Pick the first that works
          -- E.g. reify (mkName "A") will pick the class A in preference
          -- to the data constructor A
        ; return (listToMaybe names) }
  where
    lookup rdr_name
      | Just n <- isExact_maybe rdr_name   -- This happens in derived code
      = return $ if isExternalName n then Just n else Nothing
      | Just (rdr_mod, rdr_occ) <- isOrig_maybe rdr_name
      = Just <$> lookupNameCache cache rdr_mod rdr_occ
      | otherwise = return Nothing

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
   , module GHC.Core.Opt.Monad
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
   , module GHC.Unit.State
   , module GHC.Unit.Module
   , module GHC.Core.Type
   , module GHC.Core.TyCon
   , module GHC.Core.Coercion
   , module GHC.Builtin.Types
   , module GHC.Driver.Types
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
   , module GHC.Types.Unique.Supply
   , module GHC.Data.FastString
   , module GHC.Tc.Errors.Hole.FitTypes   -- for hole-fit plugins
   , -- * Getting 'Name's
     thNameToGhcName
   )
where

-- Plugin stuff itself
import GHC.Driver.Plugins

-- Variable naming
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence  hiding  ( varName {- conflicts with Var.varName -} )
import GHC.Types.Name     hiding  ( varName {- reexport from OccName, conflicts with Var.varName -} )
import GHC.Types.Var
import GHC.Types.Id       hiding  ( lazySetIdInfo, setIdExported, setIdNotExported {- all three conflict with Var -} )
import GHC.Types.Id.Info

-- Core
import GHC.Core.Opt.Monad
import GHC.Core
import GHC.Types.Literal
import GHC.Core.DataCon
import GHC.Core.Utils
import GHC.Core.Make
import GHC.Core.FVs
import GHC.Core.Subst hiding( substTyVarBndr, substCoVarBndr, extendCvSubst )
       -- These names are also exported by Type

-- Core "extras"
import GHC.Core.Rules
import GHC.Types.Annotations

-- Pipeline-related stuff
import GHC.Driver.Session
import GHC.Unit.State

-- Important GHC types
import GHC.Unit.Module
import GHC.Core.Type hiding {- conflict with GHC.Core.Subst -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import GHC.Core.Coercion hiding {- conflict with GHC.Core.Subst -}
                ( substCo )
import GHC.Core.TyCon
import GHC.Builtin.Types
import GHC.Driver.Types
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
import GHC.Types.Unique.Supply
import GHC.Types.Unique ( Unique, Uniquable(..) )
import GHC.Data.FastString
import Data.Maybe

import GHC.Iface.Env    ( lookupOrigIO )
import GHC.Prelude
import GHC.Utils.Monad  ( mapMaybeM )
import GHC.ThToHs       ( thRdrNameGuesses )
import GHC.Tc.Utils.Env ( lookupGlobal )

import GHC.Tc.Errors.Hole.FitTypes

import qualified Language.Haskell.TH as TH

{- This instance is defined outside GHC.Core.Opt.Monad.hs so that
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

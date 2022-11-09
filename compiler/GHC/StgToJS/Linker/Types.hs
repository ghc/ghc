{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- for Ident's Binary instance

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Linker.Types
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module GHC.StgToJS.Linker.Types
  ( GhcjsEnv (..)
  , newGhcjsEnv
  , JSLinkConfig (..)
  , defaultJSLinkConfig
  , generateAllJs
  , LinkedObj (..)
  , LinkableUnit
  )
where

import GHC.StgToJS.Object

import GHC.Unit.Types
import GHC.Utils.Outputable (hsep,Outputable(..),text,ppr)

import Data.Map.Strict      (Map)
import qualified Data.Map.Strict as M
import Data.Set             (Set)

import Control.Concurrent.MVar

import System.IO

import Prelude

--------------------------------------------------------------------------------
-- Linker Config
--------------------------------------------------------------------------------

data JSLinkConfig = JSLinkConfig
  { lcNoJSExecutables    :: Bool
  , lcNoHsMain           :: Bool
  , lcOnlyOut            :: Bool
  , lcNoRts              :: Bool
  , lcNoStats            :: Bool
  }

-- | we generate a runnable all.js only if we link a complete application,
--   no incremental linking and no skipped parts
generateAllJs :: JSLinkConfig -> Bool
generateAllJs s = not (lcOnlyOut s) && not (lcNoRts s)

defaultJSLinkConfig :: JSLinkConfig
defaultJSLinkConfig = JSLinkConfig
  { lcNoJSExecutables = False
  , lcNoHsMain        = False
  , lcOnlyOut         = False
  , lcNoRts           = False
  , lcNoStats         = False
  }

--------------------------------------------------------------------------------
-- Linker Environment
--------------------------------------------------------------------------------

-- | A @LinkableUnit@ is a pair of a module and the index of the block in the
-- object file
type LinkableUnit = (Module, Int)

-- | An object file that's either already in memory (with name) or on disk
data LinkedObj
  = ObjFile   FilePath      -- ^ load from this file
  | ObjLoaded String Object -- ^ already loaded: description and payload

instance Outputable LinkedObj where
  ppr = \case
    ObjFile fp    -> hsep [text "ObjFile", text fp]
    ObjLoaded s o -> hsep [text "ObjLoaded", text s, ppr (objModuleName o)]

data GhcjsEnv = GhcjsEnv
  { linkerArchiveDeps :: MVar (Map (Set FilePath)
                                   (Map Module (Deps, DepsLocation)
                                   , [LinkableUnit]
                                   )
                              )
  }

-- | return a fresh @GhcjsEnv@
newGhcjsEnv :: IO GhcjsEnv
newGhcjsEnv = GhcjsEnv <$> newMVar M.empty

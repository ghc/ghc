{-# LANGUAGE CPP #-}
{-
   GHCJS can generate multiple code variants of a Haskell module
-}
module Compiler.Variants where

import           Data.ByteString       (ByteString)
import           Data.Set              (Set)

import           Compiler.Settings

import qualified Gen2.Generator        as Gen2
import qualified Gen2.Linker           as Gen2
import qualified Gen2.Object           as Gen2
import Prelude

import           CostCentre            (CollectedCCs)
import           DynFlags              (DynFlags)
import           Module                (Module (..), InstalledUnitId)
import           StgSyn                (StgBinding, StgTopBinding)
import           HscTypes              (SptEntry, ForeignStubs)

data Variant = Variant
    { variantRender            :: GhcjsSettings
                               -> DynFlags
                               -> Module
                               -> [StgTopBinding]
                               -> [SptEntry]
                               -> ForeignStubs
                               -> CollectedCCs
                               -> ByteString
    , variantLink              :: DynFlags
                               -> GhcjsEnv
                               -> GhcjsSettings
                               -> FilePath                     -- output directory
                               -> [FilePath]                   -- include paths for home package
                               -> [InstalledUnitId]            -- dependencies
                               -> [LinkedObj]                  -- object files
                               -> [FilePath]                   -- extra JavaScript files
                               -> (Gen2.Fun -> Bool)           -- function to use as roots
                               -> Set Gen2.Fun                 -- extra roots
                               -> IO ()
    }

gen2Variant :: Variant
gen2Variant = Variant Gen2.generate Gen2.link

type StgPgm = [StgBinding]

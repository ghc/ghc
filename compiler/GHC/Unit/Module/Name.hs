{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Unit.Module.Name
  ( -- * The ModuleName type
    ModuleName
  , ModuleNamePs
  , ModuleNameP(..)

    -- * Pass conversion
  , rnModuleName

    -- * Construction
  , mkModuleName
  , mkModuleNameFS
  , parseModuleName

    -- * Deconstruction
  , moduleNameFS
  , moduleNameString
  , moduleNameSlashes
  , moduleNameColons

    -- * Comparison
  , stableModuleNameCmp
  ) where

import GHC.Prelude.Basic

import GHC.Data.FastString

import GHC.Hs.Extension.Pass (GhcPass, GhcPs, GhcRn, GhcTc)
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Module.Name

import Data.Char (isAlphaNum)
import Data.Data
import Data.String (IsString(..))
import qualified Text.ParserCombinators.ReadP as Parse
import System.FilePath

-- The textual representation GHC chooses for a module name at every 'GhcPass'.
type instance XCModuleName (GhcPass p) = FastString
type instance XXModuleName (GhcPass p) = DataConCantHappen

deriving instance Data ModuleNamePs
deriving instance Data (ModuleNameP GhcRn)
deriving instance Data (ModuleNameP GhcTc)

-- | The concrete 'ModuleName' representation used by GHC.
--
-- Module names are pass-invariant on the GHC side (a 'FastString' at every
-- 'GhcPass'), so a single 'GhcRn'-tagged type serves as /the/ standalone
-- module name.
type ModuleName = ModuleNameP GhcRn

-- | Transient 'Module' used in parser specific contexts.
type ModuleNamePs = ModuleNameP GhcPs

instance Ord (ModuleNameP (GhcPass p)) where
  nm1 `compare` nm2 = stableModuleNameCmp nm1 nm2

instance IsString (ModuleNameP (GhcPass p)) where
  fromString = mkModuleName

stableModuleNameCmp :: ModuleNameP (GhcPass p) -> ModuleNameP (GhcPass p) -> Ordering
-- ^ Compares module names lexically, rather than by their 'Unique's
stableModuleNameCmp n1 n2 = moduleNameFS n1 `lexicalCompareFS` moduleNameFS n2

moduleNameFS :: ModuleNameP (GhcPass p) -> FastString
moduleNameFS (ModuleName mod) = mod

moduleNameString :: ModuleNameP (GhcPass p) -> String
moduleNameString = unpackFS . moduleNameFS

mkModuleName :: String -> ModuleNameP (GhcPass p)
mkModuleName s = ModuleName (mkFastString s)

mkModuleNameFS :: FastString -> ModuleNameP (GhcPass p)
mkModuleNameFS s = ModuleName s

-- | Retag a module name at another pass.  Since every 'GhcPass' shares the
-- same 'FastString' payload this is a cheap conversion.  It is deliberately
-- /not/ exported: consumers should use the direction-specific 'rnModuleName'.
convertModuleName :: ModuleNameP (GhcPass p) -> ModuleNameP (GhcPass q)
convertModuleName = mkModuleNameFS . moduleNameFS

-- | Advance a module name from the parser output to the renamer output.
rnModuleName :: ModuleNamePs -> ModuleName
rnModuleName = convertModuleName

-- | Returns the string version of the module name, with dots replaced by slashes.
--
moduleNameSlashes :: ModuleName -> String
moduleNameSlashes = dots_to_slashes . moduleNameString
  where dots_to_slashes = map (\c -> if c == '.' then pathSeparator else c)

-- |Returns the string version of the module name, with dots replaced by colons.
--
moduleNameColons :: ModuleName -> String
moduleNameColons = dots_to_colons . moduleNameString
  where dots_to_colons = map (\c -> if c == '.' then ':' else c)

parseModuleName :: Parse.ReadP (ModuleNameP (GhcPass p))
parseModuleName = fmap mkModuleName
                $ Parse.munch1 (\c -> isAlphaNum c || c `elem` "_.'")

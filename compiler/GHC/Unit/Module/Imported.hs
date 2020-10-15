module GHC.Unit.Module.Imported
   ( ImportedMods
   , ImportedBy (..)
   , ImportedModsVal (..)
   , importedByUser
   )
where

import GHC.Prelude

import GHC.Unit.Module

import GHC.Types.Name.Reader
import GHC.Types.SafeHaskell
import GHC.Types.SrcLoc

-- | Records the modules directly imported by a module for extracting e.g.
-- usage information, and also to give better error message
type ImportedMods = ModuleEnv [ImportedBy]

-- | If a module was "imported" by the user, we associate it with
-- more detailed usage information 'ImportedModsVal'; a module
-- imported by the system only gets used for usage information.
data ImportedBy
    = ImportedByUser ImportedModsVal
    | ImportedBySystem

importedByUser :: [ImportedBy] -> [ImportedModsVal]
importedByUser (ImportedByUser imv : bys) = imv : importedByUser bys
importedByUser (ImportedBySystem   : bys) =       importedByUser bys
importedByUser [] = []

data ImportedModsVal = ImportedModsVal
   { imv_name        :: ModuleName
      -- ^ The name the module is imported with

   , imv_span        :: SrcSpan
      -- ^ the source span of the whole import

   , imv_is_safe     :: IsSafeImport
      -- ^ whether this is a safe import

   , imv_is_hiding   :: Bool
      -- ^ whether this is an "hiding" import

   , imv_all_exports :: !GlobalRdrEnv
      -- ^ all the things the module could provide.
      --
      -- NB. BangPattern here: otherwise this leaks. (#15111)

   , imv_qualified   :: Bool
      -- ^ whether this is a qualified import
   }


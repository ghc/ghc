-- | Where 'EpsStore.LowMem' keeps the declarations of loaded interfaces.
module EpsStore.DeclStore
  ( DeclStore (..)
  , DeclKey
  ) where

import GHC.Iface.Syntax ( IfaceDecl )
import GHC.Unit.Module ( Module, ModLocation )

-- | Identifies a declaration in a 'DeclStore'. Chosen by the store.
type DeclKey = Int

-- | A place outside the Haskell heap for the declarations of loaded
-- interfaces.
data DeclStore = DeclStore
  { ds_name  :: String
  , ds_put   :: Module -> ModLocation -> [IfaceDecl] -> IO [DeclKey]
      -- ^ Store the declarations of a module. Returns one key per
      -- declaration, in order. Called at most once per module, but possibly
      -- concurrently for different modules.
  , ds_get   :: DeclKey -> IO IfaceDecl
      -- ^ Get a declaration back. Must be thread-safe.
  , ds_close :: IO ()
  }

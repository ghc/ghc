{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Boot modules
module GHC.Unit.Module.Boot
  ( IsBootInterface (..)
  , GenWithIsBoot (..)
  ) where

import GHC.Prelude

import GHC.Utils.Binary
import GHC.Utils.Outputable

import Data.Data

-- Note [Boot Module Naming]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- Why is this section here? After all, these modules are supposed to be about
-- ways of referring to modules, not modules themselves. Well, the "bootness" of
-- a module is in a way part of its name, because 'import {-# SOURCE #-} Foo'
-- references the boot module in particular while 'import Foo' references the
-- regular module. Backpack signatures live in the normal module namespace (no
-- special import), so they don't matter here. When dealing with the modules
-- themselves, however, one should use not 'IsBoot' or conflate signatures and
-- modules in opposition to boot interfaces. Instead, one should use
-- 'DriverPhases.HscSource'. See Note [HscSource types].

-- | Indicates whether a module name is referring to a boot interface (hs-boot
-- file) or regular module (hs file). We need to treat boot modules specially
-- when building compilation graphs, since they break cycles. Regular source
-- files and signature files are treated equivalently.

data IsBootInterface = NotBoot | IsBoot
  deriving (Eq, Ord, Show, Data)

instance Binary IsBootInterface where
  put_ bh ib = put_ bh $
    case ib of
      NotBoot -> False
      IsBoot -> True
  get bh = do
    b <- get bh
    return $ case b of
      False -> NotBoot
      True -> IsBoot

data GenWithIsBoot a = GenWithIsBoot
  { gwib_mod :: a
  , gwib_isBoot :: IsBootInterface
  } deriving ( Eq, Ord, Show
             , Functor, Foldable, Traversable
             )

instance Binary a => Binary (GenWithIsBoot a) where
  put_ bh (GenWithIsBoot x y) = put_ bh (x, y)
  get bh = do
    (x, y) <- get bh
    pure $ GenWithIsBoot x y

instance Outputable a => Outputable (GenWithIsBoot a) where
  ppr (GenWithIsBoot m b) = hsep $ ppr m : case b of
    IsBoot -> []
    NotBoot -> [text "{-# SOURCE #-}"]

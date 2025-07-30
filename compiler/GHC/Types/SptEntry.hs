module GHC.Types.SptEntry
  ( SptEntry(..)
  )
where

import GHC.Builtin.Types
import GHC.Types.Id
import GHC.Types.Name
import GHC.Fingerprint.Type    ( Fingerprint )
import GHC.Prelude
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Utils.Panic.Plain

-- | An entry to be inserted into a module's static pointer table.
-- See Note [Grand plan for static forms] in "GHC.Iface.Tidy.StaticPtrTable".
data SptEntry = SptEntry Id Fingerprint

instance Outputable SptEntry where
  ppr (SptEntry id fpr) = ppr id <> colon <+> ppr fpr

instance Binary SptEntry where
  get bh = do
    nm <- get bh
    fp <- get bh
    -- static pointer logic only uses the associated Name without Type
    pure $ SptEntry (mkVanillaGlobal nm anyTy) fp

  put_ bh (SptEntry var fp) = do
    massert $ isGlobalId var
    put_ bh (getName var) *> put_ bh fp

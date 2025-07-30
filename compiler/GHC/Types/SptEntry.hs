module GHC.Types.SptEntry
  ( SptEntry(..)
  )
where

import GHC.Types.Name          ( Name )
import GHC.Fingerprint.Type    ( Fingerprint )
import GHC.Prelude
import GHC.Utils.Binary
import GHC.Utils.Outputable

-- | An entry to be inserted into a module's static pointer table.
-- See Note [Grand plan for static forms] in "GHC.Iface.Tidy.StaticPtrTable".
data SptEntry = SptEntry !Name !Fingerprint

instance Outputable SptEntry where
  ppr (SptEntry n fpr) = ppr n <> colon <+> ppr fpr

instance Binary SptEntry where
  get bh = do
    nm <- get bh
    fp <- get bh
    pure $ SptEntry nm fp
  put_ bh (SptEntry nm fp) = do
    put_ bh nm *> put_ bh fp

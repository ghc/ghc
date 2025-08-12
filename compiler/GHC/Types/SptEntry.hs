module GHC.Types.SptEntry
  ( SptEntry(..)
  )
where

import GHC.Types.Name          ( Name )
import GHC.Fingerprint.Type    ( Fingerprint )
import GHC.Utils.Outputable

-- | An entry to be inserted into a module's static pointer table.
-- See Note [Grand plan for static forms] in "GHC.Iface.Tidy.StaticPtrTable".
data SptEntry = SptEntry !Name !Fingerprint

instance Outputable SptEntry where
  ppr (SptEntry n fpr) = ppr n <> colon <+> ppr fpr

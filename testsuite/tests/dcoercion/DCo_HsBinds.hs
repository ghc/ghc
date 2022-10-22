{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module DCo_HsBinds where

import Prelude

data GhcPass p where
  GhcPs :: GhcPass Int
  GhcRn :: GhcPass Float
  GhcTc :: GhcPass Bool

type HsPatSynDetails pass = [RecordPatSynField pass]
data RecordPatSynField pass = RecordPatSynField ()

-----------------------------------------

class Outputable a where
  methD :: a -> String

instance Outputable (HsPatSynDetails (GhcPass r)) where
  methD details = ppr_v =<< details
    where
      ppr_v v = case undefined :: GhcPass r of
        GhcPs -> methD v
        GhcRn -> methD v
        GhcTc -> methD v

instance Outputable (RecordPatSynField a) where
  methD (RecordPatSynField v) = methD v

instance Outputable () where
  methD _ = "()"

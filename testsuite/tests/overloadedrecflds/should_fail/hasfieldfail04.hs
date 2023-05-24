{-# LANGUAGE DataKinds, PolyKinds, FlexibleInstances, MultiParamTypeClasses,
             TypeFamilies, GADTs #-}

import Data.Kind
import Data.Proxy
import GHC.Records (HasField(..))
import GHC.TypeLits

data S = MkS { quux :: Int }

-- S has fields, so a variable field name is not okay
instance forall (a :: Symbol). HasField a S Int where
  getField = quux
instance forall k (a :: k). HasField a S Int where
  getField = quux

-- however this should be OK as it definitely doesn't clash
-- with existing built-in instances, because of the kind mismatch
instance forall (a :: Bool). HasField a S Int where
  getField = quux

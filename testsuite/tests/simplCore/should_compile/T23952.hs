{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- The Lint failure in in #23952 is very hard to trigger.
-- The test case fails with GHC 9.6, but not 9.4, 9.8, or HEAD.
-- But still, better something than nothing.

module T23952 where

import T23952a
import Data.Proxy
import Data.Kind

type Filter :: Type -> Type
data Filter ty = FilterWithMain Int Bool

new :: forall n . Eq n => () -> Filter n
{-# INLINABLE new #-}
new _ = toFilter

class FilterDSL x where
  toFilter :: Filter x

instance Eq c => FilterDSL c where
  toFilter = case (case fromRep cid == cid of
                     True -> FilterWithMain cid False
                     False -> FilterWithMain cid True
                  ) of FilterWithMain c x -> FilterWithMain (c+1) (not x)
            where cid :: Int
                  cid = 3
  {-# INLINE toFilter #-}

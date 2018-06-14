{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Bug where

import Data.Kind

class C (a :: k) where
  type T k :: Type

instance C Left where
  type T (a -> Either a b) = Int

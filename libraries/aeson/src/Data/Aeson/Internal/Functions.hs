{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Functions
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable

module Data.Aeson.Internal.Functions
    (
      mapHashKeyVal
    , mapKeyVal
    , mapKey
    ) where

import Prelude.Compat

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M

-- | Transform a 'M.Map' into a 'H.HashMap' while transforming the keys.
mapHashKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2)
              -> M.Map k1 v1 -> H.HashMap k2 v2
mapHashKeyVal fk kv = M.foldrWithKey (\k v -> H.insert (fk k) (kv v)) H.empty
{-# INLINE mapHashKeyVal #-}

-- | Transform the keys and values of a 'H.HashMap'.
mapKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2)
          -> H.HashMap k1 v1 -> H.HashMap k2 v2
mapKeyVal fk kv = H.foldrWithKey (\k v -> H.insert (fk k) (kv v)) H.empty
{-# INLINE mapKeyVal #-}

-- | Transform the keys of a 'H.HashMap'.
mapKey :: (Eq k2, Hashable k2) => (k1 -> k2) -> H.HashMap k1 v -> H.HashMap k2 v
mapKey fk = mapKeyVal fk id
{-# INLINE mapKey #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-full-laziness  #-}

module T17904
    ( difference
    , differenceWith
    ) where

import GHC.Exts ( TYPE, Int (..)  )
import Prelude hiding (lookup)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

class Hashable a where
    hashWithSalt :: Int -> a -> Int

data Leaf k v = L k v

data HashMap k v
    = Empty
    | Leaf Word (Leaf k v)

lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
lookup k m = case lookupCont (\_ -> (# (# #) | #)) (\v _i -> (# | v #)) (fromIntegral $ (hashWithSalt undefined) k) k m of
  _ -> undefined
{-# INLINE lookup #-}

lookupCont ::
  forall rep (r :: TYPE rep) k v.
     Eq k
  => ((# #) -> r)
  -> (v -> Int -> r)
  -> Word
  -> k -> HashMap k v -> r
lookupCont _absent _present _h0 _k0 _m0 = go undefined undefined undefined undefined
  where
    go :: Word -> k -> Int -> HashMap k v -> r
    go h k _ _
        | h == undefined && k == undefined = undefined
        | otherwise          = undefined

difference :: (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
difference a b = foldlWithKey' go undefined a
  where
    go _m k _v = case lookup k b of
                 Nothing -> undefined
                 _       -> undefined

differenceWith :: (Eq k, Hashable k) => a -> HashMap k v -> HashMap k w -> HashMap k v
differenceWith _f a b = foldlWithKey' go undefined a
  where
    go _m k _v = case lookup k b of
                 Nothing -> undefined
                 _  -> undefined

foldlWithKey' :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey' _f = go
  where
    go _z Empty          = undefined
    go _z (Leaf _ _)     = undefined

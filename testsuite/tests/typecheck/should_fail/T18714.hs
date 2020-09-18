{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T18714 where

import GHC.Exts

type Id a = a

type F = Id (Any :: forall a. Show a => a -> a)

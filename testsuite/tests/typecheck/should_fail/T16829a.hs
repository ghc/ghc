{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
module T16829a where

import GHC.Exts

newtype T :: TYPE IntRep where
  MkT :: Int# -> T

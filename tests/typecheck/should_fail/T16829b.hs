{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
module T16829b where

import GHC.Exts

data family T :: TYPE IntRep
newtype instance T :: TYPE IntRep where
  MkT :: Int# -> T

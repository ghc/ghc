{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
module T13154a where

import GHC.Exts

class C1 a
deriving instance C1 (a -> b)

class C2 (a :: TYPE 'IntRep)
deriving instance C2 Int#

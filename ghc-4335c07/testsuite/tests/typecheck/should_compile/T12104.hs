{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}

module T12104 where

import GHC.TypeLits

type family F a where
  F a = TypeError (Text "error")

err :: F ()
err = ()

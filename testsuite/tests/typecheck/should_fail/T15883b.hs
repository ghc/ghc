{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnliftedNewtypes #-}

module T15883b where

import GHC.Exts

newtype Foo rep = MkFoo (forall (a :: TYPE rep). a)
deriving stock instance Eq (Foo LiftedRep)

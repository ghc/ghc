{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnliftedNewtypes #-}

module T15883c where

import GHC.Exts

newtype Foo rep = MkFoo (forall (a :: TYPE rep). a)
deriving stock instance Ord (Foo LiftedRep)

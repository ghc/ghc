{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module T18863d where

import Data.Kind

type D :: forall j -> forall i.   (i -> j) -> Type
data D :: forall j -> forall i -> (i -> j) -> Type

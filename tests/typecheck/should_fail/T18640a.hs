{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module T18640a where

import Data.Kind

type F2 :: forall a b. Type -> a
type family F2 @a :: forall b. Type -> Type where

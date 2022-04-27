{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T15343 where

import Data.Kind

elimSing :: forall (p :: forall z. z). p
elimSing = undefined

data WhySym :: Type -> Type

hsym = elimSing @WhySym

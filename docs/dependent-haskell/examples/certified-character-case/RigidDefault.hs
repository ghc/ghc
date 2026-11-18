{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module RigidDefault where

import Data.Type.Equality ((:~:)(Refl))
import Upper (Upper)

rigid :: forall (c :: Char). Upper c :~: c
rigid = Refl

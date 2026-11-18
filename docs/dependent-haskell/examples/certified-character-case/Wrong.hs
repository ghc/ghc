{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module Wrong where

import Data.Type.Equality ((:~:)(Refl))
import Upper (Upper)

wrong :: Upper 'x' :~: 'Y'
wrong = Refl

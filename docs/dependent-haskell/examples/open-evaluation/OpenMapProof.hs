{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module OpenMapProof (eachEmpty, eachCons) where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, U, Multiplicity(Many))
import OpenMapSource (Each)

-- | An empty list needs no callback application.
eachEmpty :: forall (f :: FUN @U 'Many Bool Bool).
             Proxy f -> Each f '[] :~: '[]
eachEmpty _ = Refl

-- | The result retains both the callback application and the recursive call.
eachCons :: forall (f :: FUN @U 'Many Bool Bool) (x :: Bool) (xs :: [Bool]).
            Proxy f -> Proxy x -> Proxy xs
            -> Each f (x ': xs) :~: (f x ': Each f xs)
eachCons _ _ _ = Refl

{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module UniformRows where

import Data.Type.Equality ((:~:)(Refl))
import Row

-- | Mapping over the empty row returns Empty.
empty :: foreach (p :: Int -> Bool)
      -> Prelude.fmap p ('Empty :: Row Bool Bool Int) :~: ('Empty :: Row Bool Bool Bool)
empty _ = Refl

-- | Mapping over a flagged row returns Empty independently of its fields.
flagged :: foreach (p :: Int -> Bool) -> foreach (flag :: Bool) -> foreach (x :: Int)
        -> Prelude.fmap p ('Flagged flag x :: Row Bool Bool Int) :~: ('Empty :: Row Bool Bool Bool)
flagged _ _ _ = Refl

-- | Mapping over a full row returns Empty independently of its fields.
both :: foreach (p :: Int -> Bool) -> foreach (x :: Int)
     -> Prelude.fmap p ('Both 'True 'False x :: Row Bool Bool Int) :~: ('Empty :: Row Bool Bool Bool)
both _ _ = Refl

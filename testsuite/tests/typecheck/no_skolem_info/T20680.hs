{-# language
    DerivingStrategies
  , DerivingVia
  , GeneralisedNewtypeDeriving
  , StandaloneDeriving
#-}

module T20690 (main) where

import GHC.Exts (TYPE)
import GHC.Generics (Rec1)
import Data.Kind (Type)

main :: IO ()
main = pure ()

class FunctorL (f :: Type -> TYPE r) where
  fmapL :: (a -> b) -> (f a -> f b)

newtype Base1 f a = Base1 { getBase1 :: f a }
  deriving newtype (Functor)

instance Functor f => FunctorL (Base1 f) where
  fmapL = fmap

deriving via (Base1 (Rec1 f)) instance FunctorL (Rec1 f)

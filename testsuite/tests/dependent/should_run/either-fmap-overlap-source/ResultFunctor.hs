module ResultFunctor (Result(..), Translate(..)) where

import Data.Kind (Type)

data Result e a = Failed e | Succeeded a

class Translate t where
    translate :: (a -> Type) -> t a -> t Type
    count :: t a -> Int

instance {-# OVERLAPPABLE #-} Translate (Result e) where
    translate _ (Failed e) = Failed e
    translate f (Succeeded a) = Succeeded (f a)
    count _ = 1

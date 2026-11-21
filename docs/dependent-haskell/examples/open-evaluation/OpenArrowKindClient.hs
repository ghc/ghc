{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
{-# LANGUAGE PolyKinds, TypeApplications, TypeFamilies, TypeOperators #-}
module OpenArrowKindClient (openKindRows, main) where
import OpenArrowKind (OpenKind)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import OpenArrowKindMatchable ()
import OpenArrowKindUnmatchable ()

-- | Use the matchable row as an arrow kind.
type family Interpret (b :: Bool) :: OpenKind
type instance Interpret 'True = Maybe

-- | Use the unmatchable row as a runtime function type.
negateFlag :: OpenKind
negateFlag True = False
negateFlag False = True

-- | Decompose a nominal equality of applications at the matchable kind.
recover :: forall (f :: OpenKind) a b. f a ~ f b => a :~: b
recover = Refl

-- | Construct the value selected by the matchable equation.
ordinary :: Interpret 'True Bool
ordinary = Just True

-- | Use the matchable row for the value and the other row for its function.
openKindRows :: Bool -> (Maybe Bool, Bool)
openKindRows flag = case recover @Maybe @Bool @Bool of Refl -> (ordinary, negateFlag flag)

-- | Evaluate both rows with an ordinary client.
main :: IO ()
main = do
  flag <- newIORef False >>= readIORef
  print (openKindRows flag)

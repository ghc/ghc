{-# LANGUAGE ExperimentalDependentTypes, MonoLocalBinds, ScopedTypeVariables, TypeFamilies #-}
module ImportedFamily (importedFamilyResults) where
import Data.Proxy (Proxy(..))
import ImportedFamilyProvider (Payload, importedFirst)

-- | Check the local producers before installing these family equations.
type instance Payload Int = ImportedFamily.keep Char Bool
type instance Payload Char = ImportedFamily.lambda Ordering
type instance Payload () = ImportedFamily.identity Char
type instance Payload Ordering = ImportedFamily.viaMaybe (Maybe Bool)
type instance Payload (Maybe Bool) = ImportedFamily.inferredMaybe Ordering

constLocal :: forall a b. a -> b -> a
constLocal x _ = x

keep :: forall a. a -> a -> a
keep x y = constLocal (importedFirst y x) x

lambda :: forall a. a -> a
lambda x = (\a _ -> a) x x

identity :: forall a. a -> a
identity x = (Prelude.id :: a -> a) x

viaMaybe :: a -> a
viaMaybe x = Prelude.maybe x Prelude.id (Just x)

inferredMaybe x = Prelude.maybe x Prelude.id (Just x)

class ReadPayload a where
  observed :: proxy a -> Payload a

instance ReadPayload Int where
  observed _ = True

-- | Check an ordinary instance method against the completed family.
result :: Payload Int
result = observed (Proxy :: Proxy Int)

lambdaResult :: Payload Char
lambdaResult = EQ

identityResult :: Payload ()
identityResult = 'x'

maybeResult :: Payload Ordering
maybeResult = Just False

inferredResult :: Payload (Maybe Bool)
inferredResult = LT

-- | An ordinary client can consume all five results through this interface.
importedFamilyResults :: (Payload Int, Payload Char, Payload (), Payload Ordering, Payload (Maybe Bool))
importedFamilyResults = (result, lambdaResult, identityResult, maybeResult, inferredResult)

{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies #-}
-- | Use a signed local function in an imported family equation.
module FamilySignature (observed, directObserved) where
import ImportedFamilyProvider (Payload)

type instance Payload Float = FamilySignature.keep Bool

-- | Check the equation that depends on a helper signature.
observed :: Payload Float
observed = True

keep :: forall a. a -> a
keep x = const x signed

signed :: Ready Bool -> Ready Bool
signed x = x

type family Ready a where
  Ready a = Envelope a

data Envelope a = Envelope a | Wrapped (Wrap a)
data Wrap a = Wrap a

type instance Payload Word = FamilySignature.direct Bool

-- | Check the equation whose function argument needs family reduction.
directObserved :: Payload Word
directObserved = True

direct :: forall a. DirectInput a -> a
direct x = x

type family DirectInput a where
  DirectInput a = a

{-# LANGUAGE ExperimentalDependentTypes, TypeFamilies #-}
module FamilySignature (observed) where
import ImportedFamilyProvider (Payload)

type instance Payload Float = FamilySignature.keep Bool

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

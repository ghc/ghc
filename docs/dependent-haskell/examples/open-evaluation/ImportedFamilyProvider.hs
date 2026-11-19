{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoExperimentalDependentTypes, TypeFamilies #-}
module ImportedFamilyProvider (Payload, hidden, Branch(..)) where

-- | Select the payload type of an ordinary argument.
type family Payload a

class Branch a where
  branch :: proxy a -> Bool

instance {-# INCOHERENT #-} Branch a where
  branch _ = False

instance Branch Bool where
  branch _ = True

-- | Read the dictionary selected for the integer payload.
hidden :: Branch (Payload Int) => proxy (Payload Int) -> Bool
hidden = branch

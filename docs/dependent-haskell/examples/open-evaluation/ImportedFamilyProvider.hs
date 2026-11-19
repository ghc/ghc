{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, NoExperimentalDependentTypes, TypeFamilies #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module ImportedFamilyProvider (Payload, hidden, Branch(..), importedFirst, importedStrict) where

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

-- | Return the first input without inspecting either input.
{-# INLINE importedFirst #-}
importedFirst :: forall a. a -> a -> a
importedFirst x _ = x

-- | Demand the first input before returning the second.
{-# INLINE importedStrict #-}
importedStrict :: forall a b. a -> b -> b
importedStrict !x y = y

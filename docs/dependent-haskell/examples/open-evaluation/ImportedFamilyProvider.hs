{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, NoExperimentalDependentTypes, TypeFamilies #-}
{-# OPTIONS_GHC -fno-unoptimized-core-for-interpreter -fno-omit-interface-pragmas #-}
module ImportedFamilyProvider
  (Payload, hidden, hiddenNested, Branch(..), importedFirst, importedStrict) where

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

-- | Hide the family inside an otherwise ordinary boxed type.
hiddenNested :: Maybe (Payload Int) -> Bool
hiddenNested _ = True

-- | Return the first input without inspecting either input.
{-# INLINE importedFirst #-}
importedFirst :: forall a. a -> a -> a
importedFirst x _ = x

-- | Demand the first input before returning the second.
{-# INLINE importedStrict #-}
importedStrict :: forall a b. a -> b -> b
importedStrict !x y = y

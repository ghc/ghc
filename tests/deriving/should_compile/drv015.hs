
-- This will generate an instance decl like:
--
--      instance (Show (f a), Show (g a)) => Show (Pair1 f g a)
--
-- Although the Haskell Report would not permit this instance if written out
-- explicitly, it does not say anything about whether it is acceptable for a
-- *derived* instance to generate it. As such, we allow this in GHC.
-- See Note [Valid 'deriving' predicate] in GHC.Tc.Validity.

module ShouldCompile where

newtype Pair1 f g a = Pair1 {unPair1 :: (f a, g a)}
  deriving (Eq, Ord, Show)

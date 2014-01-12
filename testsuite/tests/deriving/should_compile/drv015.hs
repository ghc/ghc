
-- July 07: I'm changing this from "should_compile" to "should_fail".
-- It would generate an instance decl like
--	insance (Show (f a), Show (g a)) => Show (Pair1 f g a)
-- and that is not Haskell 98.  
--
-- See Note [Exotic derived instance contexts] in TcSimplify.
-- The rule is simple: the context of a derived instance decl must
-- contain constraints of form (C tyvar) only, just as H98.

module ShouldCompile where

newtype Pair1 f g a = Pair1 {unPair1 :: (f a, g a)}
  deriving (Eq, Ord, Show)

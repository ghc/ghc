{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RequiredTypeArguments, TypeAbstractions #-}

module T24810 where

import GHC.TypeLits

-------------------------
-- Example from the ticket
showKnownChar :: forall c -> KnownChar c => IO ()
showKnownChar c = print (charSing @c)

withKnownChar_rta :: SChar c -> (forall c' -> c ~ c' => KnownChar c => r) -> r
withKnownChar_rta (SChar @c) r = r c

-- no type signature for example
example = withKnownChar_rta (SChar @'a') (\c -> showKnownChar c)


-------------------------
-- Example with deeper nested skolemisation needed
ex2 :: forall c. (forall a -> Eq a => forall b. (a ~ [b]) => c) -> c
ex2 = ex2

-- no type signature for foo
foo = ex2 (\c -> True)

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module T14422 where

class C f where
  foo :: f a -> ()

pattern P :: C f => f a
pattern P <- (foo -> ())

{-# COMPLETE P #-}

f :: C f => f a -> ()
f P = () -- A complete match

-- But we also have to be able to constrain applicability of a COMPLETE sig.
-- Hence another example:

class D f where
  bar :: f a -> ()

pattern Q :: D f => f a
pattern Q <- (bar -> ())

instance D [] where
  bar _ = ()
{-# COMPLETE Q :: [] #-}

g :: D f => f a -> ()
g Q = () -- Should warn! The sig shouldn't apply in a polymorphic context.

h :: [a] -> ()
h Q = () -- A complete match

-- What currently isn't possible (although, yet):
class D f => E f where
  -- Law: every match on 'Q' is COMPLETE

-- Commented out, because it's invalid syntax ATM.
-- {-# COMPLETE Q :: E f => f a #-}

i :: E f => f a -> ()
i Q = () -- Would be a complete match with GHC proposal #400


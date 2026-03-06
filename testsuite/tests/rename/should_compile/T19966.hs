{-# OPTIONS -fdefer-out-of-scope-variables #-}
{-# LANGUAGE ScopedTypeVariables #-}

module T19966 where

import Data.Proxy

-- "I" is out of scope, but we accept the definition
-- thanks to -fdefer-out-of-scope-variables.
ex1 = I

-- "Bool" is in scope, but it is a type constructor and
-- cannot be used as a term.
--
-- The -fdefer-out-of-scope-variables flag plays a double duty
-- and recovers from this failure, too.
ex2 = Bool

data T =
  Boo1 -- we want this to be suggested instead of Bool in ex2

-- IO is out of scope, so out-of-scope should be a warning, but fails.
-- "a" is in scope, but it is a type variable and
-- cannot be used as a term.
--
-- The -fdefer-out-of-scope-variables flag permits this, too.
ex3 :: forall a. Proxy a -> Bool
ex3 _ = not a

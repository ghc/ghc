{-# LANGUAGE FlexibleContexts #-}
module T10715 where

import Data.Coerce (coerce, Coercible)
import Data.Ord ( Down )  -- convenient newtype

data X a

-- See #10715 for a long discussion about whether
-- this should be accepted or not.
--
-- But in #12466 we decided to accept contradictory
-- type signatures, so definition is now accepted even
-- though you can never call it.  Instead we get a
-- redundant pattern-match warning, in the
-- post-typechecking pattern-match checks

doCoerce :: Coercible a (X a) => a -> X a
doCoerce = coerce

-- Note (30 Jun 2021). This doesn't appear contradictory,
-- so I'm moving this file out of should_fail and putting it
-- in should_compile. See below for why:

newtype N = MkN (X N)

x :: X N
x = doCoerce (MkN undefined)

-- This usage of doCoerce typechecks, and so the Coercible constraint
-- wasn't actually unsatisfiable, as it turns out. As such, the warning
-- seems to have been incorrect.

-- Here's a slightly less trivial, but similar usage:

data Y a = Y a

newtype M = MkM (Y M)

doCoerce' :: Coercible a (Y a) => a -> Y a
doCoerce' = coerce

y :: Y M
y = doCoerce' (MkM y)

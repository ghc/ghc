-- We test that leading whitespace gets properly dropped (or not!)
-- from codeblocks
module Bug201 where

-- |
-- @
-- This leading whitespace
-- should be dropped
-- @
f :: ()
f = ()

{-|
@
 But this one
 should not
@

> this should
> be dropped

@
 and so should this
 because there's a space before closing @
 @
-}
g :: ()
g = ()

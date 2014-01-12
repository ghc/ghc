-- Trac #3323
module T3323 where

import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals

-- The point here is that Handle__ is an existential type, 
-- so the haDevice field can't be updated.
--
-- The bug was that, haDevice is a "naughty" selector, we 
-- couldn't find its type constructor.  
-- 
-- This only happened when you go via an interface file, which is why
-- this test imports an existential.  To make the test more
-- standalone, you'd need to make it a two-module test

f :: Handle__ -> Handle__
f h = h {haDevice=undefined}

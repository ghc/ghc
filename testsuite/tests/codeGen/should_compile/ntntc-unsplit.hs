{-# LANGUAGE MagicHash #-}
module M (bar) where

import GHC.Exts

-- Without tables-next-to-code, the native code generator used to split the
-- proc points of a proc into separate procs (`_blk_` procs in -ddump-cmm).
-- The self tail call makes a loop header that is reached from the entry and
-- from the continuation of the call to g, so it became a proc point too: g
-- and x went on the stack at the loop header, and the call's frame kept the
-- dead slot of x, StackRep [False, True]. Now the native code generator keeps
-- the proc unsplit, as with tables-next-to-code: no `_blk_` procs, and the
-- call's frame saves g alone, StackRep [False].
bar :: (Int# -> Int#) -> Int# -> Int#
bar g x = case g x of r -> case r ># 0# of 1# -> bar g r; _ -> r

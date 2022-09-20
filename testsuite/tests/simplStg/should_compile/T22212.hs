{-# LANGUAGE MagicHash, UnboxedTuples #-}

module T22212 where

import GHC.Exts

isNullAddr# :: Addr# -> (##)
isNullAddr# a =
  case eqAddr# a nullAddr# of
    1# -> (##)
    _  -> compareBytes (##)
{-# INLINE isNullAddr# #-}

compareBytes :: (##) -> (##)
compareBytes _ = (##)
{-# NOINLINE compareBytes #-}

mArray :: forall {rep :: RuntimeRep} {res :: TYPE rep}
       .  (  () -> () -> () -> () -> ()
          -> () -> () -> () -> () -> ()
          -> () -> () -> () -> () -> ()
          -> () -> () -> () -> () -> ()
          -> () -> () -> () -> () -> ()
          -> res )
       -> res
mArray cont =
  case isNullAddr# nullAddr# of
    (##) ->
      cont
        () () () () ()
        () () () () ()
        () () () () ()
        () () () () ()
        () () () () ()
          -- As of writing this test,
          -- 9 arguments were required to trigger the bug.

{-
Original reproducer:

data Sort = MkSort BS.ByteString [()]

pattern Array :: () -> () -> Sort
pattern Array x y = MkSort "Array" [x,y]
-}
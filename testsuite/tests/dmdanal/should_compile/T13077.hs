{-# LANGUAGE MagicHash #-}
module T13077 where
import GHC.Exts

data X = A | B | C

data T = MkT !X Int# Int#

f (MkT x 0# _) = True
f (MkT x n  _)  = let v = case x of
                         A -> 1#
                         B -> 2#
                         C -> n
                  in f (MkT x v v)
 -- Tests evaluatedness for worker args

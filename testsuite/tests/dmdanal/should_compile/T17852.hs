{-# LANGUAGE MagicHash, UnliftedNewtypes #-}
module T17852 where
import GHC.Exts (Int#)

newtype T = T Int#

f :: T -> Int# -> T -> T
f a _ _ = a
{-# NOINLINE f #-} -- to force worker/wrappering

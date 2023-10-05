{-# LANGUAGE UnliftedDatatypes, UnliftedNewtypes, MagicHash #-}
module T21519a where

import GHC.Exts

type T :: UnliftedType
data T = MkT Int
MkT _ = MkT 10           -- (1)

f x = let MkT _ = MkT 10 in True

newtype T2 = MkT2 Int#
MkT2 _ = MkT2 10#        -- (2)

g x = let MkT2 _ = MkT2 10# in True

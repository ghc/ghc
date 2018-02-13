{-# LANGUAGE MagicHash #-}

module T14626 where

import GHC.Prim

data T = MkT !Bool

f v = case v of
         MkT y -> dataToTag# y

-- This should /not/ produce an inner case on the y, thus:
--    f v = case v of
--            MkT y -> case y of z -> dataToTag# z
-- But it was!  See Trac #14626 comment:4

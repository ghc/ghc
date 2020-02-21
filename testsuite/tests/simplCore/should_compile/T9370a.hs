{-# OPTIONS_GHC -O0 -ddump-inlinings #-}
module T9370a where

{-  We should see no inlinings emitted by -ddump-inlinings since we have
    specified -O0. -}

import T9370b

a m n = (b m) ++ (b n)

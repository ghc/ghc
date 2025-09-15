{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module T20347 where

import GHC.Exts

foo0 x = 10# +# (negateInt# x)
foo1 x = (10# +# x) +# (negateInt# x)
foo2 x = 10# -# (negateInt# x)
foo3 x y = (negateInt# x) *# (negateInt# y)
foo4 x = 10# *# (negateInt# x)

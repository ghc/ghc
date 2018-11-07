{-# LANGUAGE MagicHash #-}

import GHC.Exts
import GHC.Prim

main :: IO ()
main = do
    print (I# (dataToTag# a))  -- used to print 0, should print 1
    print (I# (dataToTag# f))  -- used to print 1 correctly

  where
    {-# NOINLINE f #-}
    f = T2
    {-# NOINLINE a #-}
    a = f

data T = T1 | T2

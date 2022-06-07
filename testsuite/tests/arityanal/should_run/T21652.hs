import GHC.Exts

f, g :: a -> a
f = g
g x = f x
{-# NOINLINE f #-}
{-# NOINLINE g #-}

-- should print done, not <<loop>>
main = lazy g `seq` putStrLn "done"

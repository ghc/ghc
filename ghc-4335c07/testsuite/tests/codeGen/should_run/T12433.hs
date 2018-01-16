f :: Int -> IO ()
f p = case p of
  0 -> return ()
  1 -> return ()
  2 -> return ()
  3 -> return ()
  4 -> return ()
  10 -> return ()
  11 -> return ()
  _ -> print p
{-# NOINLINE f #-}

main = f 8

main :: IO ()
main = do let xs = [1..1000000]
          let ys = [1..2000000]
          print $ {-# SCC "last_xs" #-} last xs
          print $ {-# SCC "last_init_xs" #-} last $ init xs
          print $ {-# SCC "last_ys" #-} last ys
          print $ {-# SCC "last_init_ys" #-}last $ init ys

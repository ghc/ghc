{-# LANGUAGE DoRec #-}

main :: IO ()
main = do x <- return (length [1 .. 42 :: Int])
          rec b <- return x
              let a = const c
              c <- print "x"
          print (b, a b)

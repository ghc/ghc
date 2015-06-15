f :: Int -> String
f n = case n of
  0x8000000000000000 -> "yes"
  _ -> "no"
{-# NOINLINE f #-}

main = do
    let string = "0x8000000000000000"
    let i = read string :: Integer
    let i' = fromIntegral i :: Int
    print i
    print i'
    print (f i')

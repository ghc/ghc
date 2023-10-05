f1 :: Int -> String
f1 n = case n of
  0 -> "bar"
  0x10000000000000000 -> "foo"
  _ -> "c"
{-# NOINLINE f1 #-}

g1 :: Int -> String
g1 n = if n == 0 then "bar" else
       if n == 0x10000000000000000 then  "foo" else
       "c"
{-# NOINLINE g1 #-}

f2 :: Int -> String
f2 n = case n of
  0x10000000000000000 -> "foo"
  0 -> "bar"
  _ -> "c"
{-# NOINLINE f2 #-}

g2 :: Int -> String
g2 n = if n == 0x10000000000000000 then  "foo" else
       if n == 0 then "bar" else
       "c"
{-# NOINLINE g2 #-}

main = do
    let i = read "0" :: Int
    print (f1 i)
    print (g1 i)
    print (f2 i)
    print (g2 i)

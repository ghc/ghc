{-# NOINLINE f #-}
f :: Int -> Int
f = {-# SCC f #-} g

{-# NOINLINE g #-}
g :: Int -> Int
g x = {-# SCC g #-} x + 1

main = {-# SCC main #-} return $! f 3


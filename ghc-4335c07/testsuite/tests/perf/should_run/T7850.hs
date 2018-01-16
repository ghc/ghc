
{-# LANGUAGE BangPatterns #-}

main :: IO ()
main = print $ ack 4 1

ack :: Int -> Int -> Int
ack 0 !n = n+1
ack m  0 = ack (m-1) 1
ack m  n = ack (m-1) $ ack m (n-1)


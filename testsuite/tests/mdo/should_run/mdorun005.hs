{-# LANGUAGE DoRec #-}

-- check that do-rec does not perform segmentation
-- compare with ../should_fail/mdofail006.hs
t :: IO [Int]
t = do rec xs <- return (1:xs)
       print (length (take 10 xs))     -- would work since out of the segment
       return (take 10 xs)

main :: IO ()
main = t >>= print

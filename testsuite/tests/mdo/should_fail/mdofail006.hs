{-# LANGUAGE DoRec #-}

-- check that do-rec does not perform segmentation
t :: IO [Int]
t = do rec xs <- return (1:xs)
           print (length (take 10 xs))     -- would diverge without segmentation
       return (take 10 xs)

-- should diverge when run
-- currently it exhibits itself via a blocked MVar operation
main :: IO ()
main = t >>= print

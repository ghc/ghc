{-# LANGUAGE RecursiveDo #-}

-- check that mdo does perform segmentation
t :: IO [Int]
t = mdo xs <- return (1:xs)
        print (length (take 10 xs))     -- would diverge without segmentation
        return (take 10 xs)

main :: IO ()
main = t >>= print

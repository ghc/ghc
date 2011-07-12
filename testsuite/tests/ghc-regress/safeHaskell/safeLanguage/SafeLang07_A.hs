{-# LANGUAGE Trustworthy #-}

-- | Here we expose a MinList API that only allows elements
-- to be inserted into a list if they are at least greater
-- than an initial element the list is created with.
module SafeLang07_A (
        MinList,
        newMinList,
        insertMinList,
        printIntMinList
    ) where

data MinList a = MinList a [a]

newMinList :: Ord a => a -> MinList a
newMinList n = MinList n []

insertMinList :: Ord a => MinList a -> a -> MinList a
insertMinList s@(MinList m xs) n | n > m     = MinList m (n:xs)
                                 | otherwise = s

printIntMinList :: MinList Int -> IO ()
printIntMinList (MinList min xs) = putStrLn $ "MinList Int :: MinList " ++ show min ++ " " ++ show xs


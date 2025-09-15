module DoExpansion2 where


-- make sure all the (>>=) expansion works okay

getVal :: Int -> IO String
getVal _ = return "x"

ffff1, ffff2, ffff3, ffff4, ffff5, ffff6, ffff7, ffff8 :: IO Int


ffff1 = do x <- getChar
           return (x + 1) -- should error here

ffff2 = do x <- (getVal 3)
           return x -- should error here

ffff3 = do x <- getChar
           y <- getChar
           return (x + y) -- should error here

ffff4 = do Just x <- getChar -- should error here
           return x


ffff5 = do x <- getChar
           Just x <- getChar  -- should error here
           return x

ffff6 = do _ <- (getVal 1)
           return ()         -- should error here


ffff7 = do Just x <- getVal 3 4 -- should error here
           return x


ffff8 = do x <- getVal 3
           return x   -- should error here

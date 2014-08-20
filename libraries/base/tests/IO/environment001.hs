import System.Environment

main = do
    var0 <- getEnv "GHC_TEST"
    putStrLn var0
    -- The length proves that we actually decoded it properly, not just read it
    -- in as latin1 or something (#3308, #3307)
    putStrLn ("Test 1: " ++ show (length var0))

    [arg0] <- getArgs
    putStrLn arg0
    putStrLn ("Test 2: " ++ show (length arg0))

    [arg1] <- withArgs ["你好!"] getArgs
    putStrLn arg1
    putStrLn ("Test 3: " ++ show (length arg1))

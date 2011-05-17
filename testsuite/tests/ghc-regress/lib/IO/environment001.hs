import System.Environment

main = do
    var0 <- getEnv "GHC_TEST"
    putStrLn var0
    -- The length proves that we actually decoded it properly, not just read it
    -- in as latin1 or something (#3308, #3307)
    print (length var0)
    
    [arg0] <- getArgs
    putStrLn arg0
    print (length arg0)
    
    [arg1] <- withArgs ["你好!"] getArgs
    putStrLn arg1
    print (length arg1)

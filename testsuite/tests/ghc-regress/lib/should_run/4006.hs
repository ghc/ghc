import System.Process

testUnicode :: String -> IO String
testUnicode str = init `fmap` (readProcess "echo" [str] "")

main = do
    testUnicode "It works here" >>= putStrLn
    testUnicode "А здесь сломалось" >>= putStrLn

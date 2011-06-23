import System.Process

testUnicode :: String -> IO String
testUnicode str = readProcess "printf" ["%s", str] ""

main = do
    testUnicode "It works here" >>= putStrLn
    testUnicode "А здесь сломалось" >>= putStrLn

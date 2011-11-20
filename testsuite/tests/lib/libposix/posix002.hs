import System.Posix.Process

main =
    executeFile "printenv" True [] (Just [("ONE","1"),("TWO","2")])

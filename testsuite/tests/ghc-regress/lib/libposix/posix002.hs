import Posix

main =
    executeFile "printenv" True [] (Just [("ONE","1"),("TWO","2")])

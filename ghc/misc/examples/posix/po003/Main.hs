import Posix

main = 
    openFile "po003.out" WriteMode >>= \ h ->
    runProcess "pwd" [] Nothing (Just "/usr/tmp") Nothing (Just h) Nothing

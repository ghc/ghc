import IO -- 1.3

main = 
    hSetBuffering stdin NoBuffering	>>
    hSetBuffering stdout NoBuffering	>>
    interact id

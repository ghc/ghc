main = 
    hSetBuffering stdin NoBuffering	>>
    hSetBuffering stdout NoBuffering	>>
    interact id

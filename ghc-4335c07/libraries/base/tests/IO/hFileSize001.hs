import System.IO

-- !!! test hFileSize

main = do
    h  <- openFile "hFileSize001.hs" ReadMode
    sz <- hFileSize h
    print sz

import LibDirectory (removeFile)

main =
    openFile "io008.in" ReadMode >>= \ hIn ->
    openFile "io008.out" ReadWriteMode >>= \ hOut ->
    removeFile "io008.out" >>
    hGetPosn hIn >>= \ bof ->
    copy hIn hOut >>
    hSetPosn bof >>
    copy hIn hOut >>
    hSeek hOut AbsoluteSeek 0 >>
    hGetContents hOut >>= \ stuff ->
    putStr stuff

copy :: Handle -> Handle -> IO ()
copy hIn hOut =
    try (hGetChar hIn) >>=
    either (\ EOF -> return ()) ( \ x -> hPutChar hOut x >> copy hIn hOut)

import IO -- 1.3
import GHCio

import Directory (removeFile)

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
    tryIO (hGetChar hIn) >>=
    either (\ err -> if isEOFError err then return () else error "copy") ( \ x -> hPutChar hOut x >> copy hIn hOut)

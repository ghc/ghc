import IO -- 1.3

import Directory

main =
    createDirectory "foo" >>
    openFile "foo/bar" WriteMode >>= \ h ->
    hPutStr h "Okay\n" >>
    hClose h >>
    renameFile "foo/bar" "foo/baz" >>
    renameDirectory "foo" "bar" >>
    openFile "bar/baz" ReadMode >>= \ h ->
    hGetContents h >>= \ stuff ->
    putStr stuff >>
    hClose h >>
    removeFile "bar/baz" >>
    removeDirectory "bar"

import IO

import Directory

main = do
    createDirectory "foo"
    h <- openFile "foo/bar" WriteMode
    hPutStr h "Okay\n"
    hClose h
    renameFile "foo/bar" "foo/baz"
    renameDirectory "foo" "bar"
    h <- openFile "bar/baz" ReadMode
    stuff <- hGetContents h
    putStr stuff
--    hClose h  -- an error !
    removeFile "bar/baz"
    removeDirectory "bar"

import Control.Exception

import System.Directory
import System.Environment
import System.IO

import Data.Char
import Data.List

import GHC.IO.Encoding

main = do
    hSetBuffering stdout NoBuffering

    -- 1) A file name arriving via an argument
    putStrLn "Test 1"
    [file] <- getArgs
    print $ map ord file
    readFile file >>= putStr

    -- 2) A file name arriving via getDirectoryContents
    putStrLn "Test 2"
    [file] <- fmap (filter ("chinese-file-" `isPrefixOf`)) $ getDirectoryContents "."
    print $ map ord file
    readFile file >>= putStr

    -- 3) A file name occurring literally in the program
    -- The file is created with a UTF-8 file name as well, so this will only work in Windows or a
    -- UTF-8 locale, or this string will be encoded in some non-UTF-8 way and won't match.
    putStrLn "Test 3"
    let file = "chinese-file-小说"
    print $ map ord file
    readFile file >>= putStr

    -- 4) A file name arriving via another file.
    -- Again, the file is created with UTF-8 contents, so we read it in that encoding.
    -- Once again, on non-Windows this may fail in a non-UTF-8 locale because we could encode the valid
    -- filename string into a useless non-UTF-8 byte sequence.
    putStrLn "Test 4"
    str <- readFileAs utf8 "chinese-name"
    let file = dropTrailingSpace str
    print $ map ord file
    readFile file >>= putStr

readFileAs :: TextEncoding -> FilePath -> IO String
readFileAs enc fp = do
    h <- openFile fp ReadMode
    hSetEncoding h enc
    hGetContents h

dropTrailingSpace :: String -> String
dropTrailingSpace = reverse . dropWhile (not . isAlphaNum) . reverse

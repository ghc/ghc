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
    -- This will only work if we are in the UTF-8 locale since the file is created
    -- on disk with a UTF-8 file name.
    putStrLn "Test 3"
    let file = "chinese-file-小说"
    print $ map ord file
    readFile file >>= putStr

    -- 4) A file name arriving via another file.
    -- In this case we have to override the default encoding
    -- so we get surrogate bytes for non-decodable namse.
    putStrLn "Test 4"
    str <- readFileAs fileSystemEncoding "chinese-name"
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

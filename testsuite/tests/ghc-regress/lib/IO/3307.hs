import Control.Exception

import System.Directory
import System.Environment
import System.IO

import Data.Char
import Data.List

import GHC.IO.Encoding

main = do
    -- 1) A file name arriving via an argument
    [file] <- getArgs
    readFile file >>= putStr
    
    -- 2) A file name arriving via getDirectoryContents
    [file] <- fmap (filter ("chinese-file-" `isPrefixOf`)) $ getDirectoryContents "."
    readFile file >>= putStr
    
    -- 3) A file name occurring literally in the program
    -- This will only work if we are in the UTF-8 locale since the file is created
    -- on disk with a UTF-8 file name.
    readFile "chinese-file-小说" >>= putStr
    
    -- 4) A file name arriving via another file.
    -- In this case we have to override the default encoding
    -- so we get surrogate bytes for non-decodable namse.
    (readFileAs fileSystemEncoding "chinese-name" >>= (readFile . dropTrailingSpace)) >>= putStr

readFileAs :: TextEncoding -> FilePath -> IO String
readFileAs enc fp = do
    h <- openFile fp ReadMode
    hSetEncoding h enc
    hGetContents h

dropTrailingSpace :: String -> String
dropTrailingSpace = reverse . dropWhile (not . isAlphaNum) . reverse

import System.IO
import System.Directory
import Data.Char

tempFile = "temp"

create :: IO ()
create = do
    h <- openFile tempFile WriteMode
    hSetEncoding h latin1
    hPutStr h [chr 128]
    hClose h

main :: IO ()
main = do
    create

    utf8Ignore <- mkTextEncoding "UTF8//IGNORE"
    h <- openFile tempFile ReadMode
    hSetEncoding h utf8Ignore
    hGetContents h >>= putStrLn

    removeFile tempFile

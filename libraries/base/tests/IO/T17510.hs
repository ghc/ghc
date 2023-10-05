-- | Test that a handle from 'hDuplicate' really shares the file position with
-- its parent.

import System.IO
import GHC.IO.Handle

main :: IO ()
main = do
    writeFile "test" "hello\nworld\ntesting\n"
    h <- openFile "test" ReadMode
    h2 <- hDuplicate h
    hGetLine h  >>= putStrLn  -- should print "hello"
    hGetLine h2 >>= putStrLn -- should print "world"
    hGetLine h  >>= putStrLn -- should print "testing"


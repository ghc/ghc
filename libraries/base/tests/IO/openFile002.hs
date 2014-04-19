import Data.Char
import System.IO

-- !!! Open a non-existent file for reading (should fail)

main = openFile "nonexistent" ReadMode

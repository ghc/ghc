import Char
import IO

-- !!! Open a non-existent file for reading (should fail)

main = openFile "nonexistent" ReadMode

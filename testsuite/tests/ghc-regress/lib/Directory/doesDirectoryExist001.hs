-- !!! "/" was not recognised as a directory in 6.0.x
import System.Directory
main = doesDirectoryExist "/" >>= print


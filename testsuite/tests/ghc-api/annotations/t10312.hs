import CheckUtils
import System.Environment( getArgs )

main::IO()
main = do
        [libdir,fileName] <- getArgs
        testOneFile libdir fileName

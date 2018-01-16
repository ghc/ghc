import System.Exit
import System.IO

main = hPutStrLn stderr "My custom Setup" >> exitFailure

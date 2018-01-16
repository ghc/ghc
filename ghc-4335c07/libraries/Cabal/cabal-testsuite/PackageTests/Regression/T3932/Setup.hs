import CabalMessage (message)
import System.Exit
import System.IO

main = hPutStrLn stderr message >> exitFailure

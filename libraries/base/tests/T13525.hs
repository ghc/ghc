import System.Posix.Files
import System.IO
import System.Timeout

main :: IO ()
main = do
    createNamedPipe "test" accessModes
    h <- openFile "test" ReadMode
    hWaitForInput h (5 * 1000)
    return ()

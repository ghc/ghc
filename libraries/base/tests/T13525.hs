import System.IO
import System.Timeout

main :: IO ()
main = do
    hWaitForInput stdin (5 * 1000)
    return ()

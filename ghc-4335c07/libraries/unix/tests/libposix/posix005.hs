import Data.List (sort)
import System.IO
import System.Posix.Env

printEnv :: IO ()
printEnv = getEnvironment >>= print . sort

main = do
    hSetBuffering stdout NoBuffering
    term <- getEnv "TERM"
    maybe (return ()) putStrLn term
    setEnvironment [("one","1"),("two","2")]
    printEnv
    setEnv "foo" "bar" True
    printEnv
    setEnv "foo" "baz" True
    printEnv
    setEnv "fu" "bar" True
    printEnv
    unsetEnv "foo"
    printEnv
    clearEnv
    printEnv


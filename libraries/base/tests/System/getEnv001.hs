
import System.Environment (getEnv)
import System.IO.Error (catchIOError, isDoesNotExistError)

main :: IO ()
main = do
    term <- getEnv "PATH"
    putStrLn "Got $PATH"
    fish <- getEnv "One fish, two fish, red fish, blue fish" `catchIOError` getEnv_except
    putStrLn fish

getEnv_except :: IOError -> IO String
getEnv_except ioe
 | isDoesNotExistError ioe = return ""
 | otherwise               = ioError ioe

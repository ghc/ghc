import System (getEnv)

import IO ( isDoesNotExistError )

main :: IO ()
main = do
    term <- getEnv "PATH"
    putStrLn "Got $PATH"
    fish <- getEnv "One fish, two fish, red fish, blue fish"  `catch` getEnv_except
    putStrLn fish

getEnv_except :: IOError -> IO String
getEnv_except ioe
 | isDoesNotExistError ioe = return ""
 | otherwise		   = ioError ioe

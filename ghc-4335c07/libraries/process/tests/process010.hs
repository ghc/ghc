
import System.IO.Error
import System.Process

main :: IO ()
main = do run "true"
          run "false"
          run "/non/existent"
          putStrLn "Done"

run :: FilePath -> IO ()
run fp = (rawSystem fp [] >>= print)
         `catchIOError` \e -> putStrLn ("Exc: " ++ show e)

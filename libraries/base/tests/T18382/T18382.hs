import System.Environment
import System.Process

main :: IO ()
main = do
    hc <- getEnv "TEST_HC"
    out <- readProcess hc ["--info"] ""
    putStrLn "done"

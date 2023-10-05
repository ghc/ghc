
import System.Environment

main :: IO ()
main = do args <- getArgs
          mapM_ (putStrLn . f) args

f :: String -> String
f str = "Arg: " ++ show str


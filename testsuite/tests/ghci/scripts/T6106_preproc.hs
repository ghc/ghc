
import Control.Concurrent
import Data.ByteString as BS
import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
              [x, y, z] -> f x y z
              _ -> error ("Bad args: " ++ show args)

f :: String -> String -> String -> IO ()
f x y z = do bs <- BS.readFile y
             BS.writeFile z bs
             threadDelay 1000000
             Prelude.writeFile x "FAIL"


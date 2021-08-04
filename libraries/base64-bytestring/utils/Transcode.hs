import qualified Data.ByteString as B
import System.Environment
import Data.ByteString.Base64

main = do
  (kind:files) <- getArgs
  let xcode bs = case kind of
            "decode" -> case decode bs of
                          Left err -> putStrLn err
                          Right p -> B.putStr p
            "decodeLenient" -> B.putStr (decodeLenient bs)
            "encode" -> B.putStr (encode bs)
            "read" -> B.putStr bs
  case files of
    [] -> B.getContents >>= xcode
    fs -> mapM_ (\f -> B.readFile f >>= xcode) fs

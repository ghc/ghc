import qualified Data.ByteString.Char8 as C

main = do
    let text = "lemon"
    C.putStrLn $ C.pack text

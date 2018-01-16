import qualified Data.ByteString.Char8 as C
import Text.PrettyPrint

main = do
    putStrLn (render (text "foo"))
    let text = "lemon"
    C.putStrLn $ C.pack text

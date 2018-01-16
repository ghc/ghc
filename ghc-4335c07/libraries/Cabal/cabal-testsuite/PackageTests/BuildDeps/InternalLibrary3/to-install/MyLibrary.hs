module MyLibrary where

import qualified Data.ByteString.Char8 as C
import Text.PrettyPrint

myLibFunc :: IO ()
myLibFunc = do
    putStrLn (render (text "foo"))
    let text = "myLibFunc installed"
    C.putStrLn $ C.pack text

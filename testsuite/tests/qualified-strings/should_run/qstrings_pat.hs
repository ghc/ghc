{-# LANGUAGE QualifiedStrings #-}

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Example.ByteStringAscii as Ascii
import qualified Example.ByteStringUtf8 as Utf8
import qualified Example.Text as Text

main :: IO ()
main = do
  let text = Text."foo" :: Text
  case text of
    Text."foo" -> putStrLn "Text.\"foo\" matched"
    _ -> putStrLn "Text.\"foo\" did not match"

  let ascii = Ascii."語" :: ByteString
  case ascii of
    Ascii."語" -> putStrLn "Ascii.\"語\" matched"
    _ -> putStrLn "Ascii.\"語\" did not match"

  let utf = Utf8."語" :: ByteString
  case utf of
    Utf8."語" -> putStrLn "Utf8.\"語\" matched"
    _ -> putStrLn "Utf8.\"語\" did not match"

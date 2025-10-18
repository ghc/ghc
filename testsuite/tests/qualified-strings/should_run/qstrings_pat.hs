{-# LANGUAGE QualifiedStrings #-}

import qualified Example.ByteStringAscii as Ascii
import qualified Example.ByteStringUtf8 as Utf8
import qualified Example.Text as Text

main :: IO ()
main = do
  case Text."foo" of
    Text."foo" -> putStrLn "Text.\"foo\" matched"
    _ -> putStrLn "Text.\"foo\" did not match"

  case Ascii."語" of
    Ascii."語" -> putStrLn "Ascii.\"語\" matched"
    _ -> putStrLn "Ascii.\"語\" did not match"

  case Utf8."語" of
    Utf8."語" -> putStrLn "Utf8.\"語\" matched"
    _ -> putStrLn "Utf8.\"語\" did not match"

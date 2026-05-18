{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QualifiedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- These are harvested from ../qualified-strings

module PprQualifiedStrings where

import Data.Typeable (Typeable, typeOf)
import qualified Example.ByteStringAscii as Ascii
import qualified Example.ByteStringUtf8 as Utf8
import qualified Example.Text as Text

exprs :: IO ()
exprs = do
  inspect "I'm a String" -- would be an ambiguous type error with OverloadedStrings
  inspect Text."I'm a Text"
  inspect Ascii."I'm an ASCII bytestring: 語"
  inspect Utf8."I'm a UTF8 bytestring: 語"

  inspect """
    I'm a multiline
    String value
    !
  """

  inspect Text."""
    I'm a multiline
    Text value
    !
  """

  inspect Text .  """
    I'm a multiline
    Text value
  """

    inspect Text .
      """
      I'm a multiline
      Text value
      """

pats :: IO ()
pats = do
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

th :: IO ()
th =
  $(do
      foldr (\stmt acc -> [| $stmt >> $acc |]) [| pure () |] $
        [ [| inspect Text."I'm a Text" |]
        , [| inspect Ascii."I'm an ASCII bytestring: 語" |]
        , [| inspect Utf8."I'm a Utf8 bytestring: 語" |]
        , [|
            inspect Text."""
              I'm a multiline
              Text string
            """
          |]
        ]
   )

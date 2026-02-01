{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QualifiedStrings #-}

import Data.Typeable (Typeable, typeOf)
import qualified Example.ByteStringAscii as Ascii
import qualified Example.ByteStringUtf8 as Utf8
import qualified Example.Text as Text

main :: IO ()
main = do
  inspect "I'm a String" -- would be an ambiguous type error with OverloadedStrings
  inspect Text."I'm a Text"
  inspect Ascii."I'm an ASCII bytestring: 語"
  inspect Utf8."I'm a UTF8 bytestring: 語"

  inspect Text."""
    I'm a multiline
    Text value
    !
  """

inspect :: (Typeable a, Show a) => a -> IO ()
inspect a = do
  putStrLn $ ">>> " ++ show a
  putStrLn $ show $ typeOf a

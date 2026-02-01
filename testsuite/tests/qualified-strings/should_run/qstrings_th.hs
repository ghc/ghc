{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QualifiedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Typeable (Typeable, typeOf)
import qualified Example.ByteStringAscii as Ascii
import qualified Example.ByteStringUtf8 as Utf8
import qualified Example.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (mkModName)

main :: IO ()
main =
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

inspect :: (Typeable a, Show a) => a -> IO ()
inspect a = do
  putStrLn $ ">>> " ++ show a
  putStrLn $ show $ typeOf a

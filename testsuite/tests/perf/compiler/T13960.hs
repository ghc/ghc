{-# LANGUAGE OverloadedStrings #-}

-- GHC used to run out of simplifier ticks due to inlining the internals of
-- `toStrict . toLazyByteString`.
module T13960 (breaks) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, stringUtf8, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.String (IsString(..))

newtype Query = Query ByteString

toByteString :: Builder -> ByteString
toByteString x = toStrict (toLazyByteString x)

instance IsString Query where
  fromString = Query . toByteString . stringUtf8

breaks :: [(Query, Query)]
breaks =
  [ ("query001a", "query001b")
  , ("query002a", "query002b")
  , ("query003a", "query003b")
  , ("query004a", "query004b")
  , ("query005a", "query005b")
  , ("query006a", "query006b")
  , ("query007a", "query007b")
  , ("query008a", "query008b")
  , ("query009a", "query009b")
  , ("query010a", "query010b")
  , ("query011a", "query011b")
  , ("query012a", "query012b")
  , ("query013a", "query013b")
  , ("query014a", "query014b")
  , ("query015a", "query015b")
  , ("query016a", "query016b")
  , ("query017a", "query017b")
  , ("query018a", "query018b")
  , ("query019a", "query019b")
  , ("query020a", "query020b")
  , ("query021a", "query021b")
  , ("query022a", "query022b")
  , ("query023a", "query023b")
  , ("query024a", "query024b")
  , ("query025a", "query025b")
  , ("query026a", "query026b")
  , ("query027a", "query027b")
  , ("query028a", "query028b")
  , ("query029a", "query029b")
  , ("query030a", "query030b")
  , ("query031a", "query031b")
  , ("query032a", "query032b")
  , ("query033a", "query033b")
  , ("query034a", "query034b")
  , ("query035a", "query035b")
  , ("query036a", "query036b")
  , ("query037a", "query037b")
  , ("query038a", "query038b")
  , ("query039a", "query039b")
  , ("query040a", "query040b")
  , ("query041a", "query041b")
  , ("query042a", "query042b")
  , ("query043a", "query043b")
  , ("query044a", "query044b")
  , ("query045a", "query045b")
  , ("query046a", "query046b")
  , ("query047a", "query047b")
  , ("query048a", "query048b")
  , ("query049a", "query049b")
  , ("query050a", "query050b")
  ]

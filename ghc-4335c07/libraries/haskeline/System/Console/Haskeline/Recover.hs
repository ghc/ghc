module System.Console.Haskeline.Recover where

import GHC.IO.Encoding
import GHC.IO.Encoding.Failure

transliterateFailure :: TextEncoding -> TextEncoding
transliterateFailure
  TextEncoding
    { mkTextEncoder = mkEncoder
    , mkTextDecoder = mkDecoder
    , textEncodingName = name
    } = TextEncoding
          { mkTextDecoder = fmap (setRecover
                                $ recoverDecode TransliterateCodingFailure)
                            mkDecoder
          , mkTextEncoder = fmap (setRecover
                                $ recoverEncode TransliterateCodingFailure)
                            mkEncoder
          , textEncodingName = name
          }
  where
    setRecover r x = x { recover = r }

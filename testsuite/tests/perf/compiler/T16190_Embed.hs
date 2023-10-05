module T16190_Embed where

import Data.Word
import Language.Haskell.TH

embedBytes :: [Word8] -> Q Exp
embedBytes bs = return (LitE (StringPrimL bs))

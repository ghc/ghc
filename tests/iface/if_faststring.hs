{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Lib
import GHC.Data.FastString
import GHC.Iface.Binary

main :: IO ()
main = do
  sz <- testSize MaximumCompression (concat (replicate 1000 ["abc", "cde", "efg" :: FastString]))
  writeFile "FULLSIZE" (show sz)
  sz <- testSize SafeExtraCompression (concat (replicate 1000 ["abc", "cde", "efg" :: FastString]))
  writeFile "MEDIUMSIZE" (show sz)
  sz <- testSize NormalCompression (concat (replicate 1000 ["abc", "cde", "efg" :: FastString]))
  writeFile "NORMALSIZE" (show sz)

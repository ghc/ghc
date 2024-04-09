{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Lib
import GHC.Data.FastString

main :: IO ()
main = do
  sz <- testSize (concat (replicate 1000 ["abc", "cde", "efg" :: FastString]))
  writeFile "SIZE" (show sz)

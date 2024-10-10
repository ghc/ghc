module Main where

import GHC.Utils.Encoding (zEncodeString,zDecodeString)
import Control.Monad

main :: IO ()
main = mapM_ test
  [ "ghc-prim_GHC.Types_$tc'(#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#)_closure"
  , "ghc-prim_GHC.Tuple_$tc'(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)2_closure"
  , "ghc-prim_GHC.Tuple_(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)_info"
  , "ghc-prim_GHC.Types_$tc'(# #)3_bytes"
  ]

test :: String -> IO ()
test s = do
  let e = zEncodeString s
  putStrLn e
  when (zDecodeString e /= s) $ do
    error $ "Invalid z-encoding roundtrip for: " ++ s

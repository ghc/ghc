module Lib where

import GHC.Utils.Binary
import GHC.Iface.Binary
import qualified Data.ByteString as B

testSize :: Binary a => a -> IO Int
testSize payload = do
  bh <- openBinMem 1024
  putWithUserData QuietBinIFace bh payload
  withBinBuffer bh (\b -> return (B.length b))


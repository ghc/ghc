module Lib where

import GHC.Utils.Binary
import GHC.Iface.Binary
import qualified Data.ByteString as B
import System.Environment
import Data.Maybe

testSize :: Binary a => CompressionIFace -> a -> IO Int
testSize compLvl payload = do
  args <- getArgs
  bh <- openBinMem 1024
  putWithUserData QuietBinIFace compLvl bh payload
  withBinBuffer bh (\b -> return (B.length b))


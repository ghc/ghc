module Haddock.InterfaceFile (
  InterfaceFile(..),
  writeInterfaceFile,
  readInterfaceFile
) where

import HaddockTypes
import Haddock.Exception

import Binary
import System.IO
import Data.Word
import qualified Data.Map as Map
import Control.Monad

data InterfaceFile = InterfaceFile {
  ifDocEnv :: DocEnv
} 

instance Binary InterfaceFile where
  put_ bh (InterfaceFile docEnv) = put_ bh (Map.toList docEnv)
  get bh = do
    envList <- get bh
    return (InterfaceFile (Map.fromList envList))

packageFileMagic = 0xDA303001 :: Word32
packageFileVersion = 0 :: Word16

writeInterfaceFile :: FilePath -> InterfaceFile -> IO ()
writeInterfaceFile filename iface = do 
  h <- openBinaryFile filename WriteMode
  bh <- openBinIO h
  ud <- newWriteState
  bh <- return $ setUserData bh ud
  put_ bh packageFileMagic
  put_ bh packageFileVersion
  put_ bh iface
  hClose h
    
readInterfaceFile :: FilePath -> IO InterfaceFile
readInterfaceFile filename = do
  h <- openBinaryFile filename ReadMode
  bh <- openBinIO h
  ud <- newReadState undefined
  bh <- return (setUserData bh ud)
  magic <- get bh
  when (magic /= packageFileMagic) $ throwE $
    "Magic number mismatch: couldn't load interface file: " ++ filename
  (version :: Word16) <- get bh
  get bh

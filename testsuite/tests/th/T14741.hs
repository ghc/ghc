{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

import Language.Haskell.TH.Lib
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import GHC.Exts
import System.Mem
import Control.Monad.IO.Class
import GHC.CString

ptr :: Ptr ()
ptr = Ptr $(do
   -- create a buffer containing the "Hello World!" string
   let xs = [72,101,108,108,111,32,87,111,114,108,100,33] :: [Word8]
   fp <- liftIO $ mallocForeignPtrArray 25
   liftIO $ withForeignPtr fp $ \p -> do
      pokeArray p xs
   -- create a "Bytes" literal with an offset and size to only include "World"
   let bys = mkBytes fp 6 5
   liftIO performGC -- check that the GC doesn't release our buffer too early
   litE (bytesPrimL bys))

main :: IO ()
main = do
  let s = case ptr of Ptr addr -> unpackNBytes# addr 5#
  putStrLn s

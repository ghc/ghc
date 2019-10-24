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
import T14741A


main :: IO ()
main = do
  let s = case ptr of Ptr addr -> unpackNBytes# addr 5#
  putStrLn s

{-# LANGUAGE TemplateHaskell #-}
module T23309 where

import Foreign.C.String
import Language.Haskell.TH
import System.IO

import T23309A

$(do runIO $ do
       cstr <- c_foo 42
       str <- peekCString cstr
       hPutStrLn stderr str
       hFlush stderr
     return [])

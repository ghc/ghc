{-# LANGUAGE TemplateHaskell #-}
module T23378 where

import Foreign.C.String
import Language.Haskell.TH
import System.IO

import T23378A

$(do runIO $ do
       hPrint stderr isatty
       hFlush stderr
     return [])

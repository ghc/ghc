{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH.Syntax
import Foreign.C.String

$(do
   -- some targets (e.g. Darwin) require a "_" symbol prefix...
#if defined(darwin_HOST_OS)
   addForeignSource LangAsm
      ".global \"_mydata\"\n\
      \_mydata:\n\
      \.ascii \"Hello world\\0\"\n"
#else
   addForeignSource LangAsm
      ".global \"mydata\"\n\
      \mydata:\n\
      \.ascii \"Hello world\\0\"\n"
#endif
   return [])

foreign import ccall "&mydata" mystring :: CString

main :: IO ()
main = putStrLn =<< peekCString mystring

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH.Syntax
import Foreign.C.String

$(do
   -- some architectures require a "_" symbol prefix...
   -- GHC defines a LEADING_UNDERSCORE CPP constant to indicate this.
   addForeignSource LangAsm
      "#if defined(LEADING_UNDERSCORE)\n\
      \.global \"_mydata\"\n\
      \_mydata:\n\
      \#else\n\
      \.global \"mydata\"\n\
      \mydata:\n\
      \#endif\n\
      \.ascii \"Hello world\\0\"\n"
   return [])

foreign import ccall "&mydata" mystring :: CString

main :: IO ()
main = putStrLn =<< peekCString mystring

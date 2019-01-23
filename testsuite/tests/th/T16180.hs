{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Main where

import Language.Haskell.TH.Syntax
import Foreign.C.String

$(do
   -- some architectures require a "_" symbol prefix...
   -- GHC defines a LEADING_UNDERSCORE CPP constant to indicate this.
   let asm = unlines
           [ "#include \"ghcconfig.h\""
           , "#if LEADING_UNDERSCORE"
           , ".global \"_mydata\""
           , "_mydata:"
           , "#else"
           , ".global \"mydata\""
           , "mydata:"
           , "#endif"
           , ".ascii \"Hello world\\0\""
           ]
   addForeignSource LangAsm asm
   return [])

foreign import ccall "&mydata" mystring :: CString

main :: IO ()
main = putStrLn =<< peekCString mystring

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH.Syntax
import Foreign.C.String
import Config -- from "ghc" package

$(do
   -- some targets (e.g. Darwin) require a "_" symbol prefix...
   addForeignSource LangAsm (if cLeadingUnderscore == "YES"
      then ".global \"_mydata\"\n\
           \_mydata:\n\
           \.ascii \"Hello world\\0\"\n"
      else ".global \"mydata\"\n\
           \mydata:\n\
           \.ascii \"Hello world\\0\"\n"
      )
   return [])

foreign import ccall "&mydata" mystring :: CString

main :: IO ()
main = putStrLn =<< peekCString mystring

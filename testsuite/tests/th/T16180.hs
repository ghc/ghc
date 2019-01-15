{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH.Syntax
import Foreign.C.String

$(addForeignSource LangAsm ".global \"mydata\"\nmydata:\n.ascii \"Hello world\\n\\0\"\n" >> return [])

foreign import ccall "&mydata" mystring :: CString

main :: IO ()
main = putStrLn =<< peekCString mystring

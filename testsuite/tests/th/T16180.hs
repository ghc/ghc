{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH.Syntax
import Foreign.C.String

$(do
   -- some targets (e.g. Darwin) require a "_" symbol prefix. Ideally we
   -- could reuse FP_LEADING_UNDERSCORE from aclocal.m4 for this
   -- somehow, but this hopefully is OK for now.
   addForeignSource LangAsm $ unlines
#if defined(wasm32_HOST_ARCH)
      [ ".section .rodata.mydata,\"\",@"
      , ".globl mydata"
      , "mydata:"
      , ".asciz \"Hello world\""
      , ".size mydata, 12"
      ]
#elif defined(darwin_HOST_OS) || (defined(mingw32_HOST_OS) && WORD_SIZE_IN_BITS == 32)
      [ ".global \"_mydata\""
      , "_mydata:"
      , ".ascii \"Hello world\\0\""
      ]
#else
      [ ".global \"mydata\""
      , "mydata:"
      , ".ascii \"Hello world\\0\""
      ]
#endif
   return [])

foreign import ccall "&mydata" mystring :: CString

main :: IO ()
main = putStrLn =<< peekCString mystring

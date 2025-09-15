{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -optc-DA_MACRO=1 -optcxx-DA_MACRO=1 #-}

import Language.Haskell.TH.Syntax
import System.IO (hFlush, stdout)

foreign import ccall fc :: Int -> IO Int

do addForeignSource LangC $ unlines
     [ "#include <stdio.h>"
     , "int fc(int x) {"
     , "  printf(\"calling f(%d)\\n\",x);"
     , "  fflush(stdout);"
     , "  return A_MACRO + x;"
     , "}"
     ]
   return []

main :: IO ()
main = do
  fc 2 >>= print
  hFlush stdout

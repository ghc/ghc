-- Tests that a reasonable error is reported when addCStub is used with
-- incorrect C code.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -optc-DA_MACRO=1 #-}

import Language.Haskell.TH.Syntax

foreign import ccall f :: Int -> IO Int

do addCStub $ unlines
     [ "#include <stdio.h>"
     , "int f(int x {"
     , "  printf(\"calling f(%d)\\n\",x);"
     , "  return A_MACRO + x;"
     , "}"
     ]
   return []

main :: IO ()
main = f 2 >>= print

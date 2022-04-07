{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -optc-DA_MACRO=1 -optcxx-DA_MACRO=1 #-}

import Language.Haskell.TH.Syntax
import System.IO (hFlush, stdout)

foreign import ccall fcxx :: Int -> IO Int

do addForeignSource LangCxx $ unlines
     [ "#include <iostream>"
     , "extern \"C\" {"
     , "  int fcxx(int x) {"
     , "    std::cout << \"calling fcxx(\" << x << \")\" << std::endl;"
     , "    std::cout.flush();"
     , "    return A_MACRO + x;"
     , "  }"
     , "}"
     ]
   return []

main :: IO ()
main = do
  fcxx 5 >>= print
  hFlush stdout

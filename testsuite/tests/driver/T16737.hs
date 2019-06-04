{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -DFOO=2 -optP=-DBAR=3 -optc=-DBAZ=5 -optcxx=-DBAZ=7 #-}

import Language.Haskell.TH.Syntax

do
  let code = unlines
        [ "#if defined(__cplusplus)"
        , "extern \"C\" {"
        , "#endif"
        , "#include <T16737.h>"
        , "int FUN(void) {"
        , "  return FOO * BAR * BAZ;"
        , "}"
        , "#if defined(__cplusplus)"
        , "}"
        , "#endif"
        ]
  addForeignSource LangC code
  addForeignSource LangCxx code
  pure []

foreign import ccall unsafe "c_value"
  c_value :: IO Int

foreign import ccall unsafe "cxx_value"
  cxx_value :: IO Int

main :: IO ()
main = do
  print =<< c_value
  print =<< cxx_value

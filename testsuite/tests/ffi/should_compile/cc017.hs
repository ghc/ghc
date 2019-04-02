{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Syntax

do
    -- must be compiled with -optc=-DC -optcxx=-DCXX
    addForeignSource LangC
        "int CXX = 0; // is not a preprocessor definition   \n\
        \_Static_assert(C == 1, \"C\");                     "
    addForeignSource LangCxx
        "auto C = 0; // is not a preprocessor definition    \n\
        \static_assert(CXX == 1, \"CXX\");                  "
    pure []

main :: IO ()
main = pure ()

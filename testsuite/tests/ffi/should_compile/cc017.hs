{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Syntax

-- Check -optc and -optcxx options passing.
-- This file must be compiled with -optc=-DC -optcxx=-DCXX

do  addForeignSource LangC
        "int CXX = 0; // -DCXX must not be passed to C          \n\
        \_Static_assert(C, \"name C must come from -DC\");      "

    addForeignSource LangCxx
        "int C = 0; // -DC must not be passed to C++            \n\
        \static_assert(CXX, \"name CXX must come from -DCXX\"); "

    pure []

main :: IO ()
main = pure ()

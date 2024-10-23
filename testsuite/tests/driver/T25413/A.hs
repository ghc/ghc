{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module A where

import Foreign.C
import Language.Haskell.TH.Syntax

$(addForeignFilePath LangC "dep.c" >> pure [])

foreign import ccall unsafe "test" c_test :: Int

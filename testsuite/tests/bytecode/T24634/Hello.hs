{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Hello where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

foreign import capi "hello_c.h say_hello" say_hello :: IO Int

foreign import ccall fromForeignFile :: Int -> IO Int

[] <$ addForeignSource LangC "int fromForeignFile(int x) { return x * 23; }"

mkHello :: DecsQ
mkHello = do
  n <- runIO say_hello
  m <- runIO (fromForeignFile n)
  [d| hello :: IO Int
      hello = return $(lift m) |]

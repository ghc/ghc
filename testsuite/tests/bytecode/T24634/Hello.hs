{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Hello where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

foreign import capi "hello.h say_hello" say_hello :: IO Int

mkHello :: DecsQ
mkHello = do
  n <- runIO say_hello
  [d| hello :: IO Int
      hello = return $(lift n) |]

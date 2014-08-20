{-	Bug report 28 May 99

When compiled with ghc-4.02, everything's fine, it outputs "Value 7" as
expected. But compiled with ghc-pre-4.03 it yields this error message.

   Fail: Prelude.read: no parse
-}

module Main where

data Msg = Value Int | Inc   deriving (Show, Read)
  
main = do let v = read "Value 7"::Msg
          print v


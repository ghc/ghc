module Main where

import TcRun038_B( Foo(..), bar )

instance Foo Int where
  op x = x+1

main = print (bar (3::Int))

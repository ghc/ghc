module Lib (greeting) where

import Lib.Internal (name)

greeting :: String
greeting = "Hello, " ++ name

module Main where

import Parse
import Type
main          =  interact (linesP (reads :: (Parse String Type)))

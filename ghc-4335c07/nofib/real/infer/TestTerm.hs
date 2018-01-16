module Main where

import Parse
import Term
main          =  interact (linesP (reads :: (Parse String Term)))

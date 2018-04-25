 {-# LANGUAGE TemplateHaskell #-}
 module Library where

import TH

main = print ($(spliceMe) [1, 2])

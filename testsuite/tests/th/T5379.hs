{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

$( [d| g = 0
       h = $( return $ LamE [VarP (mkName "g")] (VarE 'g) ) |] )
       	 -- The 'g should bind to the g=0 definition

-- Should print 0, not 1!
main = print (h 1)

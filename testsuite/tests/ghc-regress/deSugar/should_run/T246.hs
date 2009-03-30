{-# LANGUAGE ScopedTypeVariables #-}
-- Test Trac #246

module Main where

import Control.Exception

data T = T { x :: Bool, y :: Bool }

f (T { y=True, x=True }) = "Odd"
f _                      = "OK"

g (T { x=True, y=True }) = "Odd2"
g _                      = "Odd3"

funny = T { x = undefined, y = False }

main = do { print (f funny)  -- Should work, because we test
       	    	     	     -- y first, which fails, and falls
			     -- through to "OK"

          ; Control.Exception.catch 
	          (print (g funny))        -- Should fail, because we test
	    	  (\(_::SomeException) -> print "caught")   -- x first, and hit "undefined"
	  }

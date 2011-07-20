{-# OPTIONS -XArrows #-}

-- Test for Trac #1662

module Arrow where

import Control.Arrow

expr' :: Arrow a => a Int Int
expr' = error "urk"

term :: Arrow a => a () Int
term = error "urk"

expr1 :: Arrow a => a () Int
expr1 = proc () -> do
          x <- term -< ()
          expr' -< x

expr2 :: Arrow a => a () Int
expr2 = proc y -> do
          x <- term -< y
          expr' -< x

{-# LANGUAGE PolyKinds #-}

module T9017 where

import Control.Arrow

foo :: a b (m b)
foo = arr return

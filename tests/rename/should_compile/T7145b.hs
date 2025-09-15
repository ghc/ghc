{-# LANGUAGE CPP #-}
module T7145b ( A.Applicative(pure) ) where

import qualified Control.Applicative as A

pure :: ()
pure = ()

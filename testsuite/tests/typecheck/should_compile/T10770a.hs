{-# LANGUAGE GHC2021 #-}
module T10770a where

import Data.Typeable

main = print $ foo $ Just ()

foo :: Typeable (t a) => t a -> String
foo x = let k = show $ typeOf x in k
